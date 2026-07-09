;;; test-mevedel-view.el -- Tests for mevedel-view -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-menu)
(require 'mevedel-transcript)
(require 'mevedel-structs)
(require 'mevedel-pipeline)
(require 'mevedel-tool-registry)
(require 'mevedel-mentions)
(require 'mevedel-skills)
(require 'mevedel-workspace)
(require 'mevedel-file-state)
(require 'mevedel-session-persistence)
(require 'mevedel-tool-ui)
(require 'mevedel-preview-mode)
(require 'mevedel-permission-queue)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-plan)
(require 'mevedel-tool-task)
(require 'mevedel-agents)
(require 'mevedel-hooks)
(require 'mevedel-review)
(require 'mevedel-view-fragment)
(require 'mevedel-view-history)

(defvar mevedel-plugin-extra-roots)
(defvar org-mode-hook)
(declare-function gptel-menu "ext:gptel-transient" ())
(declare-function org-entry-put "org" (pom property value))

(defun test-mevedel-view--raw-bytes (&rest bytes)
  "Return BYTES as an Emacs string of raw byte characters."
  (apply #'string (mapcar #'unibyte-char-to-multibyte bytes)))


;;
;;; Test helpers

(defmacro mevedel-view-test--with-buffers (&rest body)
  "Execute BODY with a data buffer and view buffer set up.
Binds `data-buf' and `view-buf' in scope.  Cleans up afterwards."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*"))
         (mevedel-user-dir (file-name-as-directory
                            (make-temp-file "mevedel-view-user-" t)))
         (mevedel-plugin-extra-roots nil))
     (unwind-protect
         (progn
           ;; Set up data buffer as a minimal gptel-like buffer
           (with-current-buffer data-buf
             (org-mode)
             (setq-local gptel-response-separator "\n\n")
             (setq-local gptel-prompt-prefix-alist '((org-mode . "*** "))))
           ;; Wire up view buffer
           (mevedel-view--setup view-buf data-buf)
           ,@body)
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t)))))

(defun mevedel-view-test--insert-data (data-buf text props)
  "Insert TEXT into DATA-BUF with gptel text property PROPS.
PROPS is the value for the `gptel' property."
  (with-current-buffer data-buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when props
        (put-text-property start (point) 'gptel props)))))

(defun mevedel-view-test--insert-composer-draft (draft &optional point-offset)
  "Insert DRAFT into the editable composer and move point by POINT-OFFSET."
  (let ((start (mevedel-view--input-start))
        (inhibit-read-only t))
    (goto-char start)
    (insert draft)
    (remove-text-properties
     start (point)
     '(read-only nil
       mevedel-view-prompt nil
       font-lock-face nil
       face nil
       front-sticky nil
       rear-nonsticky nil))
    (goto-char (+ start (or point-offset (length draft))))))

(defun mevedel-view-test--count-substring (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (let ((count 0)
        (start 0)
        position)
    (while (setq position (string-search needle text start))
      (cl-incf count)
      (setq start (+ position (length needle))))
    count))

(defun mevedel-view-test--capf-candidates (capf &optional prefix)
  "Return completion candidates from CAPF for PREFIX."
  (all-completions (or prefix "") (nth 2 capf)))

(defun mevedel-view-test--skill-hint-string ()
  "Return the current skill argument hint overlay string."
  (and (overlayp mevedel-view--skill-argument-hint-overlay)
       (overlay-buffer mevedel-view--skill-argument-hint-overlay)
       (overlay-get mevedel-view--skill-argument-hint-overlay
                    'after-string)))

(defun mevedel-view-test--write-skill (dir name frontmatter)
  "Create DIR/NAME/SKILL.md with FRONTMATTER."
  (let* ((skill-dir (file-name-as-directory (file-name-concat dir name)))
         (skill-file (file-name-concat skill-dir "SKILL.md")))
    (make-directory skill-dir t)
    (with-temp-file skill-file
      (insert "---\n")
      (insert frontmatter)
      (unless (string-suffix-p "\n" frontmatter)
        (insert "\n"))
      (insert "---\n"))
	    skill-file))

(defun mevedel-view-test--stop-prompt-hook (_event)
  "Block prompt submission in view-send cases."
  '(:continue nil :stop-reason "blocked"))

(defvar mevedel-view-test--seen-prompt nil)

(defun mevedel-view-test--rewrite-prompt-hook (event)
  "Capture prompt EVENT and rewrite it in view-send cases."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"))

(defun mevedel-view-test--rewrite-prompt-hook-with-context (event)
  "Capture prompt EVENT, rewrite it, and add model-only context."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :additional-context "model-only context"))

(defun mevedel-view-test--rewrite-prompt-hook-with-message (event)
  "Capture prompt EVENT, rewrite it, and add a user-facing message."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"
    :system-message "changed by test hook"))


;;
;;; Segment extraction

(mevedel-deftest mevedel-transcript--extract-segments ()
  ,test
  (test)
  :doc "single user segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'user (caar segs))))))

  :doc "user + response segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi there\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
        (should (= 2 (length segs)))
        (should (eq 'user (caar segs)))
        (should (eq 'response (caadr segs))))))

  :doc "response + tool + response segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Some response\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nresult\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "More response\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
        (should (= 3 (length segs)))
        (should (eq 'response (caar segs)))
        (should (eq 'tool (caadr segs)))
        (should (eq 'response (car (caddr segs)))))))

  :doc "response table continuation gaps stay in the response"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "| Name" 'response)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert " | Role |\n")
        (remove-text-properties start (point) '(gptel nil))))
    (mevedel-view-test--insert-data
     data-buf
     "|------|------|\n| Alice | Engineer |\n"
     'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'response (caar segs))))))

  :doc "expands partial start/end to full gptel runs"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Some response\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n(:name \"Grep\" :args (:pattern \"foo\"))\n\nmatch\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "More response\n" 'response)
    (with-current-buffer data-buf
      ;; Simulate incremental rerender entering in the middle of the tool run.
      (let* ((tool-start (next-single-property-change (point-min) 'gptel))
             (mid-start (+ tool-start 2))
             (mid-end (+ tool-start 10))
             (segs (mevedel-transcript--extract-segments mid-start mid-end)))
        (should (= 1 (length segs)))
        (pcase-let ((`(,kind ,seg-start ,_seg-end) (car segs)))
          (should (eq 'tool kind))
          (should (eq ?\n (char-after seg-start)))
        (should (string-prefix-p "\n(:name \"Grep\""
                                   (buffer-substring-no-properties seg-start (+ seg-start 20))))))))

  :doc "repairs restored tool blocks and adjacent response fragments"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (mevedel-view-test--insert-data data-buf "Assistant intro.\n" 'response)
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
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
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
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user response) (mapcar #'car segs)))
          (should (= (cadr (car segs)) block-start))
          (should (= (caddr (car segs)) prefix-start))
          (should (string-prefix-p
                   "Green loop completed"
                   (buffer-substring-no-properties
                    (cadr (cadr segs)) (caddr (cadr segs)))))))))
  :doc "keeps queued user batches in user segments when splitting prefixes"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (block-start prefix-start response-start)
        (setq block-start (point))
        (insert "<system-reminder>\n"
                "Queued messages arrived.\n"
                "</system-reminder>\n\n"
                "<queued-user-message-batch count=\"1\">\n"
                "<queued-user-message index=\"1\">\n"
                "Please keep going.\n"
                "</queued-user-message>\n"
                "</queued-user-message-batch>\n")
        (setq prefix-start (point))
        (insert "Conti")
        (setq response-start (point))
        (insert "nuing the answer.\n")
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user response) (mapcar #'car segs)))
          (should (= (cadr (car segs)) block-start))
          (should (= (caddr (car segs)) prefix-start))
          (should (string-prefix-p
                   "Continuing the answer"
                   (buffer-substring-no-properties
                    (cadr (cadr segs)) (caddr (cadr segs)))))))))
  :doc "does not treat literal tool markers as tool blocks without a tool run"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "Text mentioning markers:\n#+begin_tool (Read :file_path \"/tmp/f\")\n(:name \"Read\")\n#+end_tool\n"
     'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'response (caar segs))))))
  :doc "preserves text after a stale tool run extends beyond end marker"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool user) (mapcar #'car segs)))
          (should (= block-end (cadr (cadr segs))))
          (should (string-match-p
                   "Tail text must survive"
                   (buffer-substring-no-properties
                    (cadr (cadr segs)) (caddr (cadr segs)))))))))
  :doc "keeps literal end-tool markers inside recovered tool results"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-end (caddr (car segs))))))))
  :doc "keeps persisted-looking tool blocks inside recovered tool results"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not extend recovered tool block to response marker text"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not extend stale spill through response marker text"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= block-end (caddr (car segs))))))))
  :doc "recovers outer block when stale bounds start at nested marker"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not cross unpropertized output after a literal close"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= nested-start (cadr (cadr segs))))
          (should (= block-end (caddr (cadr segs))))))))
  :doc "keeps same tool run across a nested-looking marker"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "splits adjacent persisted tool blocks sharing a stale tool prop"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments
                     (point-min) (point-max))))
          (should (equal '(tool tool) (mapcar #'car segs)))
          (should (= first-start (cadr (car segs))))
          (should (= first-end (caddr (car segs))))
          (should (= second-start (cadr (cadr segs))))
          (should (= second-end (caddr (cadr segs))))))))
  :doc "does not extend stale tool block across mailbox delivery"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments
                     (point-min) (point-max))))
          (should (equal '(tool user tool) (mapcar #'car segs)))
          (should (= first-start (cadr (car segs))))
          (should (= first-end (caddr (car segs))))
          (should (= second-start (cadr (caddr segs))))
          (should (= second-end (caddr (caddr segs))))
          (should (string-match-p
                   "verifier--tool-gap"
                   (buffer-substring-no-properties
                    (cadr (cadr segs)) (caddr (cadr segs)))))))))
  :doc "does not cross a blank unpropertized gap after a literal close"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= nested-start (cadr (cadr segs))))
          (should (= block-end (caddr (cadr segs))))))))
  :doc "recovers outer block when stale bounds start after a nested marker"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not merge a previous unpropertized block into a later tool"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= real-start (cadr (cadr segs))))
          (should (= real-end (caddr (cadr segs))))))))
  :doc "does not merge a prose gap before a later unpropertized tool"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= real-start (cadr (cadr segs))))
          (should (= real-end (caddr (cadr segs))))))))
  :doc "does not cross response text before a stale tool run"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(user response tool) (mapcar #'car segs)))
          (should (= real-start (cadr (caddr segs))))
          (should (= real-end (caddr (caddr segs))))))))
  :doc "recovers a structural close line misclassified as response"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "recovers response-marked close when body lost tool property"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "recovers response-marked close after a literal output marker"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "recovers response-marked close after unclassified body text"
  (mevedel-view-test--with-buffers
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
        (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "does not swallow legitimate user prompt between tool and response"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (tool-start response-start)
        (setq tool-start (point))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call_1"))
        (insert "please explain that output\n")
        (setq response-start (point))
        (insert "sure, here is the explanation\n")
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segments (mevedel-transcript--extract-segments
                         (point-min) (point-max))))
          (should (equal '(tool user response)
                         (mapcar #'car segments)))))))

  :doc "ignore segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "thinking...\n" 'ignore)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'ignore (caar segs)))))))


;;
;;; Turn grouping

(mevedel-deftest mevedel-view--group-into-turns ()
  ,test
  (test)
  :doc "single user turn"
  (let* ((segs '((user 1 10)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 1 (length turns)))
    (should (eq 'user (plist-get (car turns) :role))))

  :doc "user then assistant turn"
  (let* ((segs '((user 1 10) (response 10 30) (tool 30 50)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 2 (length (plist-get (cadr turns) :segments)))))

  :doc "multiple user-assistant pairs"
  (let* ((segs '((user 1 10) (response 10 20) (user 20 30) (response 30 40)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "reasoning text (nil segments) inside assistant turn absorbed"
  (let* ((segs '((user 1 10) (ignore 10 20) (user 20 40) (tool 40 80)
                 (user 80 90) (ignore 90 100) (response 100 150)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (= 6 (length (plist-get (cadr turns) :segments)))))

  :doc "mid-turn nil gap after response absorbed when next is ignore/tool"
  (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                 (ignore 60 80) (tool 80 120) (response 120 200)))
         (turns (mevedel-view--group-into-turns segs)))
    (should (= 2 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    ;; All 5 non-user segments belong to one assistant turn
    (should (= 5 (length (plist-get (cadr turns) :segments)))))

  :doc "nil gap after response starts new user turn when next is response"
  (let* ((segs '((user 1 10) (response 10 50) (user 50 60)
                 (response 60 100)))
         (turns (mevedel-view--group-into-turns segs)))
    ;; user | assistant(response) | user | assistant(response)
    (should (= 4 (length turns)))
    (should (eq 'user (plist-get (car turns) :role)))
    (should (eq 'assistant (plist-get (cadr turns) :role)))
    (should (eq 'user (plist-get (caddr turns) :role)))
    (should (eq 'assistant (plist-get (cadddr turns) :role))))

  :doc "blank nil gap between response ranges stays in assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\n" nil)
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript--extract-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns)))
        (should (= 3 (length (plist-get (car turns) :segments)))))))

  :doc "real user prompt between response ranges remains a user turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\nSecond prompt.\n\n" nil)
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript--extract-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "real user prompt after response is not absorbed before reasoning"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf "\n\nSecond prompt.\n\n#+begin_reasoning\n" nil)
    (mevedel-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript--extract-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (equal '(assistant user assistant)
                       (mapcar (lambda (turn) (plist-get turn :role))
                               turns))))))

  :doc "scaffolding-only gap after response is still absorbed"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf "\n\n#+begin_reasoning\n" nil)
    (mevedel-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-transcript--extract-segments (point-min) (point-max)))
             (turns (mevedel-view--group-into-turns segments data-buf)))
        (should (= 1 (length turns)))
        (should (eq 'assistant (plist-get (car turns) :role)))))))


;;
;;; Tool one-liner generation

(mevedel-deftest mevedel-view--tool-one-liner ()
  ,test
  (test)
  :doc "Read tool summary"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/home/user/src/main.el\"))\n\nline 1\nline 2\nline 3\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Read" summary))
        (should (string-match-p "main\\.el" summary))
        (should (string-match-p "3 lines" summary))
        (should (string-match "✓" summary))
        (should (eq 'mevedel-view-tool-marker
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary)))
        (should (string-match "Read" summary))
        (should (eq 'mevedel-view-tool-name
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

  :doc "Bash tool summary"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"ls -la\"))\n\noutput\n"
     '(tool . "call_2"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Bash" summary))
        (should (string-match-p "ls -la" summary)))))

  :doc "hook-blocked tools show the blocking event and reason"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Bash\" :args (:command \"rm -rf /tmp/x\"))\n\nError: blocked by PreToolUse: blocked rm -rf test\n"
     '(tool . "call_hook_block"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match-p "Bash" summary))
        (should (string-match-p "rm -rf /tmp/x" summary))
        (should (string-match-p "blocked by PreToolUse: blocked rm -rf test"
                                summary))
        (should (string-match "!" summary))
        (should (eq 'mevedel-view-tool-warning
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary))))))

  :doc "fallback on unparseable content"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "not a valid sexp" '(tool . "call_3"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (stringp summary))
        (should (> (length summary) 0)))))

  :doc "fallback suppresses marker-only tool fragments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "#+begin_tool (Read :file_path \"x\")\n" '(tool . "call_4"))
    (with-current-buffer data-buf
      (should-not (mevedel-view--tool-one-liner
                   data-buf (point-min) (point-max)))))

  :doc "tool-level errors use warning marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/missing.el\"))\n\nError: File does not exist\n"
     '(tool . "call_error"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (string-match "!" summary))
        (should (eq 'mevedel-view-tool-warning
                    (get-text-property (match-beginning 0)
                                       'font-lock-face summary)))))))

(mevedel-deftest mevedel-view--tool-call-parse-media-fallback ()
  ,test
  (test)
  :doc "text Read keeps copied persisted media side-channel visible"
  (let* ((tmpdir (make-temp-file "mevedel-view-copied-media-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (copied (substring-no-properties
                  (mevedel-pipeline--format-media-data-block
                   media session nil "toolu_original"))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/copied.txt\"))\n\n"
                   "plain text" copied)
           '(tool . "toolu_other"))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search mevedel-pipeline--media-data-open
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read keeps copied side-channel visible without current tool id"
  (let* ((tmpdir (make-temp-file "mevedel-view-media-no-id-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (copied (substring-no-properties
                  (mevedel-pipeline--format-media-data-block
                   media session nil "toolu_original"))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   "plain text" copied)
           '(tool . nil))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search mevedel-pipeline--media-data-open
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read keeps copied side-channel before generated side-channel"
  (let* ((tmpdir (make-temp-file "mevedel-view-media-copied-prefix-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (copied-media '((:path "/tmp/copied.png"
                          :mime "image/png"
                          :kind image
                          :data "copied")))
         (actual-media '((:path "/tmp/a.png"
                          :mime "image/png"
                          :kind image
                          :data "actual")))
         (copied (substring-no-properties
                  (mevedel-pipeline--format-media-data-block
                   copied-media session nil "toolu_copied")))
         (actual (substring-no-properties
                  (mevedel-pipeline--format-media-data-block
                   actual-media session nil "toolu_actual")))
         (result (concat "plain text" copied "\nbody tail" actual)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   result)
           '(tool . "toolu_wrong"))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (string-search mevedel-pipeline--media-data-open
                                     (plist-get parsed :result)))
              (should (string-search "body tail"
                                     (plist-get parsed :result))))))
      (delete-directory tmpdir t)))

  :doc "media Read can strip resumed duplicate block with wrong gptel id"
  (let* ((tmpdir (make-temp-file "mevedel-view-media-duplicate-" t))
         (ws (mevedel-workspace--create :root tmpdir))
         (save-path (file-name-as-directory
                     (file-name-concat tmpdir ".mevedel" "sessions" "main")))
         (session (mevedel-session--create
                   :name "main" :workspace ws :save-path save-path))
         (media '((:path "/tmp/a.png"
                   :mime "image/png"
                   :kind image
                   :data "captured")))
         (result (substring-no-properties
                  (concat "plain media"
                          (mevedel-pipeline--format-media-data-block
                           media session nil "toolu_actual")))))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (mevedel-view-test--insert-data
           data-buf
           (concat "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
                   result)
           '(tool . "toolu_wrong"))
          (with-current-buffer data-buf
            (let ((parsed (mevedel-view--tool-call-parse
                           data-buf (point-min) (point-max))))
              (should (equal "plain media" (plist-get parsed :result))))))
      (delete-directory tmpdir t))))


;;
;;; Rendering

(mevedel-deftest mevedel-view--render-response ()
  ,test
  (test)
  :doc "renders user + assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello world\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi! How can I help?\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Hello world" text))
        (should (string-match-p "How can I help" text)))))

  :doc "clears stale compaction lock on final response"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello world\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi!\n" 'response)
    (with-current-buffer data-buf
      (setq-local mevedel--compaction-in-flight t)
      (mevedel-view--render-response (point-min) (point-max))
      (should-not mevedel--compaction-in-flight)))

  :doc "final response renders durable worked footer and hides side channel"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "worked-footer"
                       :root temporary-file-directory
                       :name "worked-footer"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 390)))
           data-turn-start response-start response-end)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil))
        (setq response-start (point)))
      (mevedel-view-test--insert-data data-buf "Done.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--start-spinner "Thinking..."))
      (with-current-buffer data-buf
        (mevedel-view--render-response response-start response-end)
        (should (string-search "request-summary"
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
      (with-current-buffer view-buf
        (should-not (mevedel-view--request-progress-visible-p))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Done" text))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "Working\\.\\.\\." text))
          (should-not (string-match-p "mevedel-render-data" text)))
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Worked for 6m" text))
          (should-not (string-match-p "mevedel-render-data" text))))))

  :doc "renders tool calls as one-liners"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should-not (string-match-p "file content" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "full rerender preserves tool result rewrite audit details"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\n"
      "updated result"
      (mevedel--format-hook-audit-record
       '(:type tool-result-rewrite
               :event "PostToolUse"
               :original-result "original result"
               :updated-result "updated result"
               :reason "redacted"))
      "\n")
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "full rerender preserves ignored tool result audit segments"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nupdated")
        (put-text-property start (point) 'gptel '(tool . "call_1")))
      (let ((start (point)))
        (insert
         (mevedel--format-hook-audit-record
          '(:type tool-result-rewrite
                  :event "PostToolUse"
                  :original-result "original result"
                  :updated-result "updated result"
                  :reason "redacted")))
        (put-text-property start (point) 'gptel 'ignore))
      (let ((start (point)))
        (insert " result\n")
        (put-text-property start (point) 'gptel '(tool . "call_1"))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text)))))

  :doc "decorates agent-result blocks inside assistant responses"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "Review update.\n"
             "<agent-result agent-id=\"reviewer--abc\" type=\"reviewer\">\n"
             "{\"findings\":[]}\n"
             "</agent-result>\n"
             "Final answer.\n")
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Review update" text))
        (should (string-match-p "✓ finished reviewer--abc" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should (string-match-p "Final answer" text))
        (should-not (string-match-p "<agent-result" text)))
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (goto-char (point-min))
      (search-forward "Review update")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✓ finished reviewer--abc" text))
        (should (string-match-p "{\"findings\":\\[\\]}" text))
        (should-not (string-match-p "<agent-result" text)))))

  :doc "renders raw Markdown responses without org-style display conversion"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "Here is `code`:\n```emacs-lisp\n(message \"hi\")\n```\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Here is `code`" text))
        (should (string-match-p "```emacs-lisp" text))
        (should (string-match-p "(message \"hi\")" text))
        (should-not (string-match-p "#\\+begin_src" text)))))

  :doc "assistant prose file line reference is buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-line-" t))
         (file (file-name-concat root "mevedel-session-persistence.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-line"
                     :root root :name "response-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-test--insert-data
             data-buf
             "See mevedel-session-persistence.el:187.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view--render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "mevedel-session-persistence.el:187")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 187 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "assistant inline code file line reference is buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-inline-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-inline-line"
                     :root root :name "response-inline-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "inline\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-test--insert-data
             data-buf
             "See `file.el:42`.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view--render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 42 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "assistant source block file line reference is not buttonized"
  (let* ((root (make-temp-file "mevedel-view-response-src-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-src-line"
                     :root root :name "response-src-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "src\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-test--insert-data
             data-buf
             "```emacs-lisp\nfile.el:42\n```\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view--render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (should-not (button-at (match-beginning 0))))))
      (delete-directory root t)))

  :doc "expanded assistant response preserves file line buttons"
  (let* ((root (make-temp-file "mevedel-view-response-expand-line-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "response-expand-line"
                     :root root :name "response-expand-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "expand\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-test--insert-data
             data-buf
             "See file.el:42.\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view--render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "file.el:42")
              (goto-char (match-beginning 0))
              (mevedel-view-toggle-section)
              (goto-char (point-min))
              (search-forward "See")
              (goto-char (match-beginning 0))
              (mevedel-view-toggle-section)
              (goto-char (point-min))
              (search-forward "file.el:42")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))
                (should (= 42 (button-get button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "renders bracket indexing literally inside response code blocks"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "```r\neval(f[[3]], df)\n```\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "```r" text))
        (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
        (should-not (string-match-p "eval(f3, df)" text)))
      (let ((pos (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "[[3]]" mevedel-view--input-marker t)
                     (match-beginning 0)))))
        (should pos)
        (should-not (get-text-property pos 'htmlize-link))
        (should-not (get-text-property pos 'help-echo))
        (should-not (get-text-property pos 'mouse-face)))))

  :doc "does not render spurious user turn for gptel tool scaffolding"
  ;; gptel inserts `#+begin_tool ... ' and `#+end_tool' around the
  ;; propertised tool content with no `gptel' property, so the
  ;; separator text between a user prompt and the tool content shows
  ;; up as a `user' segment.  Must not render as a second "You" turn.
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "\n\n#+begin_tool (Read :file_path \"/tmp/test.el\")\n"
     nil)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/test.el\"))\n\nfile content\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "\n#+end_tool\n" nil)
    (mevedel-view-test--insert-data data-buf "Here is the file.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let* ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker))
             (you-count (cl-count-if (lambda (line) (string= line "You"))
                                     (split-string text "\n"))))
        (should (= 0 you-count))
        (should-not (string-match-p "#\\+begin_tool" text))
        (should (string-match-p "Read.*test\\.el" text))
        (should (string-match-p "Here is the file" text)))))

  :doc "restored stale tool bounds render and expand without garbled fragments"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "RecoverRead"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "RecoverRead: /tmp/f"
                        :body result
                        :initially-collapsed-p t))))
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property (+ block-start 20) (- block-end 12)
                           'gptel '(tool . "call_1"))
        (insert "Fixed the byte-compilation warning.\n")
        (save-excursion
          (search-backward "ation warning")
          (setq response-start (point)))
        (put-text-property response-start (point) 'gptel 'response))
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "RecoverRead: /tmp/f" text))
        (should (string-match-p
                 "Fixed the byte-compilation warning" text))
        (should-not (string-match-p "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text))
        (should-not (string-match-p "^You$\\|Thinking" text)))
      (goto-char (point-min))
      (search-forward "RecoverRead: /tmp/f")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "file body" text))
        (should-not (string-match-p
                     "#\\+begin_tool\\|#\\+end_tool\\|n_tool" text)))))

  :doc "renders repeated read calls as individual tool rows"
  (mevedel-view-test--with-buffers
    (dotimes (i 4)
      (mevedel-view-test--insert-data
       data-buf
       (format "(:name \"Read\" :args (:file_path \"/tmp/file%d.el\"))\n\ncontent %d\n"
               i i)
       `(tool . ,(format "call_%d" i))))
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p "Reading 4 files" text))
        (dolist (file '("file0.el" "file1.el" "file2.el" "file3.el"))
          (should (string-match-p
                   (format "Read: .*%s" (regexp-quote file))
                   text))))))

  :doc "renders user turn when in-flight marker outlives echoed user block"
  (mevedel-view-test--with-buffers
    (let (start end)
      (mevedel-view-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (mevedel-view--render-response (point-min) (point-max))
        (setq start (point-max)))
      (mevedel-view-test--insert-data data-buf "*** Second\n" nil)
      (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view--render-response start end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Second" text))
          (should (string-match-p "Second response" text))))))

  :doc "does not duplicate visible send-path user echo after flag is consumed"
  (mevedel-view-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max)))
      (mevedel-view-test--insert-data data-buf "*** Second\n" nil)
      (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer data-buf
        (setq end (point-max)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Second")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view--render-response start end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Second response" text))))))

  :doc "final response includes reasoning before hook start"
  (mevedel-view-test--with-buffers
    (let (data-turn-start response-start response-end)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (mevedel-view-test--insert-data
       data-buf
       "#+begin_reasoning\nroot cause thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view--render-response response-start response-end))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Bash" text))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))
          (should-not (string-match-p "root cause thought" text)))
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "root cause thought" text))))))

  :doc "final response widening does not duplicate visible user echo"
  (mevedel-view-test--with-buffers
    (let (data-turn-start response-start response-end)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-test--insert-data
       data-buf
       "#+begin_reasoning\nlate thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer data-buf
        (setq response-start (point)))
      (mevedel-view-test--insert-data data-buf "Final answer.\n" 'response)
      (with-current-buffer data-buf
        (setq response-end (point)))
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--user-pre-rendered nil))
      (with-current-buffer data-buf
        (mevedel-view--render-response response-start response-end))
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Thinking" text))
          (should (string-match-p "Final answer" text))))))

  :doc "second-turn incremental render stays above interaction zone without duplication"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-test--insert-data data-buf "First response.\n" 'response)
      (with-current-buffer data-buf
        (mevedel-view--render-response (point-min) (point-max))
        (goto-char (point-max))
        (insert "\n\n*** Second\n")
        (setq data-turn-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--interaction-register
         (list :kind 'permission
               :id 'permission
               :count 1
               :body "\npermission\n"
               :keymap (make-sparse-keymap)
               :help-echo "Permission"
               :entry 'permission-entry
               :activate #'ignore))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "Second"))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--render-incremental data-buf)
        (mevedel-view--render-incremental data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n")))
               (partial-count
                (cl-loop with start = 0
                         while (string-match "Partial response" text start)
                         count t
                         do (setq start (match-end 0))))
               (second-pos (string-match "You\nSecond" text))
               (partial-pos (string-match "Partial response" text)))
          (should (= 2 assistant-count))
          (should (= 1 partial-count))
          (should second-pos)
          (should partial-pos)
          (should (< second-pos partial-pos))))))

  :doc "incremental render suppresses modification hooks"
  (mevedel-view-test--with-buffers
    (let (data-turn-start
          (changes 0))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Partial response.\n" 'response)
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (setq mevedel-view--pending-tool-calls
              (list (cons 'read "Calling Read…")))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--render-incremental data-buf)
        (should (= 0 changes))
        (should (string-match-p
                 "Partial response"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))

  :doc "does not duplicate the original user turn after mailbox insertion"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Simulate the real send-path marker landing inside the nil
        ;; user-property run, which `--extract-segments' expands
        ;; backward to the beginning of the prompt.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-test--insert-data data-buf "Thinking\n" 'ignore)
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (insert "✉ from explorer\nhi\n\n")
                (setq mevedel-view--in-flight-turn-start
                      (copy-marker (point) nil))
                (insert "Assistant\nold live tail\n")
                (set-marker mevedel-view--status-marker (point))
                (set-marker mevedel-view--interaction-marker (point))
                (set-marker mevedel-view--input-marker (point)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--user-pre-rendered nil)
        (mevedel-view--render-incremental data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (you-count (cl-count-if (lambda (line) (string= line "You"))
                                       (split-string text "\n"))))
          (should (= 1 you-count))
          (should (string-match-p "Assistant text" text))))))

  :doc "renders thinking blocks as summaries"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "line 1\nline 2\nline 3\n" 'ignore)
    (mevedel-view-test--insert-data data-buf "The answer is 42.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Thinking" text))
        (should-not (string-match-p "line 1" text))
        (should (string-match-p "42" text)))))

  :doc "trims contaminated thinking source to structural reasoning block"
  (mevedel-view-test--with-buffers
    (let (reasoning-start)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Bash\" :args (:command \"true\"))\n\nok\n"
       '(tool . "call_1"))
      (mevedel-view-test--insert-data
       data-buf
       "**Output observed:**\n  `ok`\n\nVERDICT: PASS\n</agent-result>\n"
       nil)
      (with-current-buffer data-buf
        (setq reasoning-start (point)))
      (mevedel-view-test--insert-data
       data-buf
       "#+begin_reasoning\nreal thought\n#+end_reasoning\n"
       'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (let ((source (get-text-property (point) 'mevedel-view-source))
              (line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (should (equal (car source) reasoning-start))
          (should (string-match-p "Thinking\\.\\.\\. (1 lines)" line)))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "real thought" text))
          (should-not (string-match-p "VERDICT: PASS" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "keeps proposed-plan tags visible outside plan mode"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'default)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data
       data-buf
       "Normal\n<proposed_plan>\n# Plan\n</proposed_plan>\nAfter\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view--render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "<proposed_plan>" text))
          (should (string-match-p "# Plan" text))))))

  :doc "strips proposed-plan tags from visible plan-mode response text"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'plan)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data
       data-buf
       "Normal\n<proposed_plan>\n# Plan\n</proposed_plan>\nAfter\n"
       'response)
      (with-current-buffer data-buf
        (mevedel-view--render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "<proposed_plan>" text))
	  (should-not (string-match-p "# Plan" text))
	  (should (string-match-p "Normal" text))
	  (should (string-match-p "After" text))))))

  :doc "strips an incomplete live proposed-plan block in plan mode"
  (mevedel-view-test--with-buffers
    (let ((session (mevedel-session--create
                    :name "test"
                    :workspace nil
                    :permission-mode 'plan)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (should (equal "Normal"
                       (mevedel-view--visible-response-text
                        "Normal\n<proposed_plan>\n# Streaming\n"))))))

  :doc "keeps historical plan-mode protocol hidden after exiting plan mode"
  (mevedel-view-test--with-buffers
    (let* ((tmp (make-temp-file "mevedel-view-plan-" t))
           (plan-path (file-name-concat tmp "plans" "current.md"))
           (old-plan-hash
            (mevedel-plan-mode--plan-hash "# Old plan\n"))
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :save-path tmp
                     :permission-mode 'default
                     :plan-metadata
                     (list :path "plans/current.md"
                           :status 'approved
                           :presented-plan-hashes
                           (list old-plan-hash)))))
      (unwind-protect
          (progn
            (make-directory (file-name-directory plan-path) t)
            (write-region "# Current plan\n" nil plan-path nil 'silent)
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (setq-local mevedel--session session))
            (mevedel-view-test--insert-data
             data-buf
             "Normal\n<proposed_plan>\n# Old plan\n</proposed_plan>\nAfter\n"
             'response)
            (with-current-buffer data-buf
              (mevedel-view--render-response (point-min) (point-max)))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should-not (string-match-p "<proposed_plan>" text))
                (should-not (string-match-p "# Old plan" text))
                (should (string-match-p "Normal" text))
                (should (string-match-p "After" text)))))
        (delete-directory tmp t))))

  :doc "renders ignored directive PROMPT drawer as collapsed user section"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Change alpha :implement:\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
     'ignore)
    (mevedel-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Implement: Change alpha" text))
        (should-not (string-match-p "Change alpha :implement:" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "Full hidden prompt" text))
        (should (string-match-p "Done" text)))
      (goto-char (point-min))
      (search-forward "Implement:")
      (should (eq 'mevedel-view-directive-action
                  (get-text-property (match-beginning 0)
                                     'font-lock-face)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Full hidden prompt" expanded)))))

  :doc "renders inline skills as compact invocation with collapsed prompt"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "*** You are helping with this user request:\n\nSay hi!\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (mevedel-pipeline--format-render-data-block
          '(:kind inline-skill
                  :name "emacs-context-snapshot"
                  :arguments "Say hi!"
                  :display-text
                  "$emacs-context-snapshot\nSay hi!")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p
                 "\\$emacs-context-snapshot\nSay hi!"
                 text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "You are helping with this user request"
                                    text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "You are helping with this user request"
                                expanded)))))

  :doc "renders nil-property inline skill metadata as compact invocation"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "*** # Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
      (mevedel-pipeline--format-render-data-block
       '(:kind inline-skill
               :name "green-loop"
               :arguments "current changes"
               :display-text "$green-loop current changes")))
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "\\$green-loop current changes" text))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "Run the loop" text))
        (should-not (string-match-p "mevedel-render-data" text)))
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "# Green Loop" expanded))
        (should (string-match-p "Run the loop" expanded)))))

  :doc "request summary before inline skill does not absorb user turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Previous answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind request-summary :elapsed-seconds 120))
     'ignore)
    (mevedel-view-test--insert-data
     data-buf
     "*** # Green Loop\n\nRun the loop.\n\nARGUMENTS: current changes"
     nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind inline-skill
              :name "green-loop"
              :arguments "current changes"
              :display-text "$green-loop current changes"))
     'ignore)
    (mevedel-view-test--insert-data data-buf "Restarting checks.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Previous answer" text))
        (should (string-match-p "Worked for 2m" text))
        (should (string-match-p "You\n\\$green-loop current changes" text))
        (should (string-match-p "Restarting checks" text))
        (should-not (string-match-p "# Green Loop" text))
        (should-not (string-match-p "mevedel-render-data" text)))))

  :doc "expanded inline skill prompt omits saved org property drawer"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     ":PROPERTIES:\n:GPTEL_PRESET: mevedel-implement\n:GPTEL_MODEL: gpt-5.5\n:GPTEL_BOUNDS: ((ignore (1 2)))\n:END:\n\n*** Skill prompt body\n\nVisible model prompt.\n"
     nil)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert
         (mevedel-pipeline--format-render-data-block
          '(:kind inline-skill
                  :name "green-loop"
                  :arguments "commits a b"
                  :display-text
                  "$green-loop\ncommits a b")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Prompt")
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Visible model prompt" expanded))
        (should-not (string-match-p ":PROPERTIES:" expanded))
        (should-not (string-match-p "GPTEL_BOUNDS" expanded)))))

  :doc "expanded external Prompt survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Change alpha :implement:\n" nil)
      (mevedel-view-test--insert-data
       data-buf
       ":PROMPT:\n## TASK\nFull hidden prompt.\n:END:\n"
       'ignore)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (mevedel-view--begin-external-turn
         "Implement: Change alpha" data-turn-start 'directive)
        (goto-char (point-min))
        (search-forward "Prompt")
        (mevedel-view-toggle-section)
        (should (string-match-p
                 "Full hidden prompt"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))
      (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Full hidden prompt" text))
          (should (string-match-p "Assistant answer" text))))))

  :doc "tolerates detached status-marker without crashing"
  ;; A detached marker passes `markerp' but `marker-position' returns
  ;; nil; downstream uses (`<=', `delete-region', `apply-collapse-states')
  ;; would otherwise signal `wrong-type-argument: integer-or-marker-p, nil'.
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi.\n" 'response)
    (with-current-buffer view-buf
      (set-marker mevedel-view--status-marker nil))
    (with-current-buffer data-buf
      (should
       (progn (mevedel-view--render-response (point-min) (point-max)) t))))

  :doc "tolerates nil status-marker without crashing"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi.\n" 'response)
    (with-current-buffer view-buf
      (setq mevedel-view--status-marker nil))
    (with-current-buffer data-buf
      (should
       (progn (mevedel-view--render-response (point-min) (point-max)) t)))))


;;
;;; Spinner

(mevedel-deftest mevedel-view--start-spinner ()
  ,test
  (test)
  :doc "creates and removes progress/request fragment"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Working...")
      (goto-char (point-min))
      (should (search-forward "Working" mevedel-view--input-marker t))
      (goto-char (match-beginning 0))
      (should (eq 'progress
                  (get-text-property
                   (point) 'mevedel-view-fragment-namespace)))
      (should (eq 'request
                  (get-text-property
                   (point) 'mevedel-view-fragment-id)))
      (should (eq mevedel-view--request-progress-region-overlay
                  (get-text-property
                   (point) 'mevedel-view-fragment-region)))
      (let ((zone-text (buffer-substring-no-properties
                        (point)
                        (mevedel-view--input-start))))
        (should (string-match-p "Working[^\n]*\n\n> \\'" zone-text)))
      (mevedel-view--stop-spinner)
      (should-not (text-property-any
                   (point-min) mevedel-view--input-marker
                   'mevedel-view-fragment-namespace 'progress))))

  :doc "request spinner stays below queued interaction text"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-queued"
                       :root temporary-file-directory
                       :name "spinner-queued"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (setf (mevedel-session-queued-user-messages session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (mevedel-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (queued (string-match-p "queued while busy" text))
               (working (string-match-p "Working" text))
               (prompt (string-match-p "\n> " text working)))
          (should queued)
          (should working)
          (should prompt)
          (should (< queued working))
          (should (< working prompt)))
        (mevedel-view--stop-spinner))))

  :doc "queued interaction rebuild suppresses modification hooks"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-queued-hooks"
                       :root temporary-file-directory
                       :name "spinner-queued-hooks"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (setf (mevedel-session-queued-user-messages session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (let ((changes 0))
          (add-hook 'after-change-functions
                    (lambda (&rest _ignore)
                      (cl-incf changes))
                    nil t)
          (mevedel-view--interaction-rebuild)
          (should (= 0 changes)))
        (mevedel-view--stop-spinner))))

  :doc "pending tool rows stay above queued text and request spinner"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-pending-queued"
                       :root temporary-file-directory
                       :name "spinner-pending-queued"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--start-spinner "Working...")
        (mevedel-view--insert-pending-tool-lines
         (list (cons "call-1" "Calling Read: a")))
        (setf (mevedel-session-queued-user-messages session)
              (list (list :input "queued while busy"
                          :display-text "queued while busy")))
        (mevedel-view--interaction-rebuild)
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (calling (string-match-p "Calling Read: a" text))
               (queued (string-match-p "queued while busy" text))
               (working (string-match-p "Working" text))
               (prompt (string-match-p "\n> " text working)))
          (should calling)
          (should queued)
          (should working)
          (should prompt)
          (should (< calling queued))
          (should (< queued working))
          (should (< working prompt)))
        (mevedel-view--stop-spinner))))

  :doc "pending tool refresh suppresses modification hooks"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((changes 0))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read…")))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--refresh-pending-tool-lines)
        (should (= 0 changes))
        (let ((pos (text-property-any
                    (point-min) mevedel-view--input-marker
                    'mevedel-view-pending-tool-live t)))
          (should pos)
          (should (eq 'history-live
                      (get-text-property
                       pos 'mevedel-view-fragment-namespace)))
          (should (equal "call-1"
                         (get-text-property
                          pos 'mevedel-view-fragment-id)))
          (should (eq mevedel-view--pending-tool-region-overlay
                      (get-text-property
                       pos 'mevedel-view-fragment-region)))
          (should (< pos (marker-position mevedel-view--status-marker)))))))

  :doc "pending tool refresh preserves composer text and point"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "draft\n> keep typing" 8)
      (let ((input-offset (- (point) (mevedel-view--input-start))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read…")))
        (mevedel-view--refresh-pending-tool-lines)
        (should (equal "draft\n> keep typing"
                       (mevedel-view--input-text)))
        (should (= (- (point) (mevedel-view--input-start))
                   input-offset)))))

  :doc "request progress render preserves selected-window history point"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (insert (propertize "Earlier answer\n"
                              'mevedel-view-type 'response
                              'mevedel-view-source '(1 . 2)))
          (set-marker-insertion-type mevedel-view--input-marker nil)
          (set-marker mevedel-view--status-marker
                      mevedel-view--input-marker)
          (set-marker mevedel-view--interaction-marker
                      mevedel-view--input-marker))
        (goto-char (point-min))
        (search-forward "Earlier")
        (goto-char (match-beginning 0))
        (let ((point-before (point)))
          (setq mevedel-view--spinner-start-time (current-time))
          (mevedel-view--ensure-request-progress data-buf)
          (should (= (window-point (selected-window)) point-before))
          (should (= (point) point-before))
          (should (looking-at-p "Earlier"))))))

  :doc "pre-tool render preserves selected-window composer point"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0))
        (with-current-buffer view-buf
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker nil))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf (copy-marker (point-min))))
          (mevedel-view-test--insert-composer-draft
           "> quoted\nsecond line" 4))
        (let ((input-offset
               (with-current-buffer view-buf
                 (- (window-point (selected-window))
                    (mevedel-view--input-start)))))
          (with-current-buffer data-buf
            (mevedel-view--pre-tool-hook
             '(:id "call-1" :name "Read" :args (:file_path "foo.el"))))
          (with-current-buffer view-buf
            (should (mevedel-view--position-in-input-region-p
                     (window-point (selected-window))))
            (should (= (- (window-point (selected-window))
                          (mevedel-view--input-start))
                       input-offset))
            (should (= (point) (window-point (selected-window))))
            (should (equal "> quoted\nsecond line"
                           (mevedel-view--input-text)))
            (should (looking-at-p "oted")))))))

  :doc "pending tool refresh keeps ordinary calling rows and adds a fragment"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char mevedel-view--input-marker)
        (insert (propertize "Calling Read...\n"
                            'mevedel-view-source '(1 . 2)
                            'mevedel-view-type 'response))
        (set-marker-insertion-type mevedel-view--input-marker t)
        (insert "| Calling Read...\n")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker (point-min) nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--pending-tool-calls
            (list (cons "call-1" "Calling Read...")))
      (mevedel-view--refresh-pending-tool-lines)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 3 (mevedel-view-test--count-substring
                      "Calling Read" text))))
      (let (occurrences)
        (goto-char (point-min))
        (while (search-forward "Calling Read" nil t)
          (push (list :source (get-text-property
                               (match-beginning 0) 'mevedel-view-source)
                      :namespace (get-text-property
                                  (match-beginning 0)
                                  'mevedel-view-fragment-namespace))
                occurrences))
        (should (cl-some (lambda (entry) (plist-get entry :source))
                         occurrences))
        (should (cl-some (lambda (entry)
                           (eq 'history-live (plist-get entry :namespace)))
                         occurrences))
        (should (cl-some (lambda (entry)
                           (and (not (plist-get entry :source))
                                (not (plist-get entry :namespace))))
                         occurrences)))))

  :doc "update replaces spinner text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--update-spinner "Calling Read...")
      (let ((text (buffer-substring-no-properties
                   (overlay-start
                    mevedel-view--request-progress-region-overlay)
                   (overlay-end
                    mevedel-view--request-progress-region-overlay))))
        (should (string-match-p "Calling Read" text))
        (should-not (string-match-p "Thinking" text)))
      (mevedel-view--stop-spinner)))

  :doc "default spinner shows working elapsed time and active agents"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-agents"
                       :root temporary-file-directory
                       :name "spinner-agents"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 12))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons "coordinator--spin"
                        '(:status running :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Thinking...")
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      mevedel-view--request-progress-region-overlay)
                     (overlay-end
                      mevedel-view--request-progress-region-overlay))))
          (should (string-match-p "Working\\.\\.\\." text))
          (should (string-match-p "[0-9]+s" text))
          (should (string-match-p "1 agent running" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner ticks replace dynamic metadata instead of appending it"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "spinner-no-pileup"
                       :root temporary-file-directory
                       :name "spinner-no-pileup"))
           (session (mevedel-session-create "main" workspace))
           (started (time-subtract (current-time) (seconds-to-time 12))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons "coordinator--spin"
                        '(:status running :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at started)))
      (with-current-buffer view-buf
        (mevedel-view--start-spinner "Thinking...")
        (dotimes (_ 3)
          (mevedel-view--spinner-tick))
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      mevedel-view--request-progress-region-overlay)
                     (overlay-end
                      mevedel-view--request-progress-region-overlay))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "agent running" text start)
                                count t
                                do (setq start (match-end 0)))))
          (should-not (string-match-p
                       "agent running.*agent running" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner tick suppresses modification hooks"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read...")))
            (changes 0))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--spinner-tick)
        (should (= 0 changes))
        (mevedel-view--stop-spinner))))

  :doc "decorated spinner status is normalized to its base label"
  (should (equal "Working..."
                 (mevedel-view--spinner-base-status
                  "Working... · 14s · 1 agent running · 21s · 2 agents running")))

  :doc "spinner marker refresh keeps the live line outside composer input"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (goto-char (point-max))
      (insert "/auto")
      (should (equal "/auto" (mevedel-view--input-text)))
      (mevedel-view--spinner-tick)
      (should (equal "/auto" (mevedel-view--input-text)))
      (mevedel-view--stop-spinner)))

  :doc "spinner tick preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view-test--insert-composer-draft "/auto")
      (let ((point-before (point)))
        (mevedel-view--spinner-tick)
        (should (equal "/auto" (mevedel-view--input-text)))
        (should (= (point) point-before)))
      (mevedel-view--stop-spinner)))

  :doc "input read excludes request progress fragment"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (goto-char (point-max))
      (insert "/auto")
      (should (equal "/auto" (mevedel-view--input-text)))))

  :doc "ASCII fallback frames can be selected"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-animate nil)
            (mevedel-view-spinner-frames mevedel-view-spinner-ascii-frames)
            (mevedel-view--spinner-frame-index 0))
        (mevedel-view--start-spinner "Working...")
        (let ((text (buffer-substring-no-properties
                     (overlay-start
                      mevedel-view--request-progress-region-overlay)
                     (overlay-end
                      mevedel-view--request-progress-region-overlay))))
          (should (string-match-p "- Working" text)))
        (mevedel-view--stop-spinner))))

  :doc "spinner tick updates pending tool frame spans"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (should (string-match-p "- Calling Read"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (mevedel-view--spinner-tick)
        (let ((frame-pos (text-property-any
                          (point-min) (point-max)
                          'mevedel-view-inline-spinner-frame t)))
          (should frame-pos)
          (should (equal (get-text-property frame-pos 'display) "+"))))))

  :doc "spinner tick does not move point on pending tool frame"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-frames '("-" "+"))
            (mevedel-view--spinner-frame-index 0)
            (mevedel-view--pending-tool-calls
             '(("call-1" . "Calling Read..."))))
        (mevedel-view--insert-pending-tool-lines
         mevedel-view--pending-tool-calls)
        (goto-char (text-property-any
                    (point-min) (point-max)
                    'mevedel-view-inline-spinner-frame t))
        (let ((point-before (point)))
          (mevedel-view--spinner-tick)
          (should (= (point) point-before))))))

  :doc "incremental response keeps request progress row visible"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-test--insert-data data-buf "Partial answer.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--insert-user-message "Prompt")
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--start-spinner "Thinking...")
        (mevedel-view--render-incremental data-buf)
        (should (mevedel-view--request-progress-visible-p))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Partial answer" text))
          (should (string-match-p "Working" text))))))

  :doc "direct data-buffer request begin starts request progress"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "direct-progress"
                       :root temporary-file-directory
                       :name "direct-progress"))
           (session (mevedel-session-create "main" workspace))
           position fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create
                     :session session
                     :started-at (current-time)))
        (setq position (copy-marker (point-max) nil))
        (setq fsm (gptel-make-fsm
                   :info (list :buffer data-buf :position position))))
      (with-current-buffer view-buf
        (setq mevedel-view--request-progress-suppressed t))
      (mevedel-view--ensure-request-progress-for-fsm fsm)
      (with-current-buffer view-buf
        (should-not mevedel-view--request-progress-suppressed)
        (should (mevedel-view--request-progress-visible-p))
        (should (markerp mevedel-view--data-turn-start))
        (should (= (marker-position mevedel-view--data-turn-start)
                   (marker-position position)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Working" text))))))

  :doc "pre-tool render keeps request progress and adds pending line"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-spinner-frames '("-" "+"))
          (mevedel-view--spinner-frame-index 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (mevedel-view--spinner-hook
         '(:name "Read" :args (:file_path "foo.el")))
        (mevedel-view--pre-tool-hook
         '(:id "call-1" :name "Read" :args (:file_path "foo.el"))))
      (with-current-buffer view-buf
        (should (mevedel-view--request-progress-visible-p))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-inline-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-pending-tool-live t))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Working" text))
          (should (string-match-p "Calling Read: foo.el" text))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Calling Read" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "spinner hook does not duplicate pending tool status in flight"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (with-current-buffer data-buf
      (mevedel-view--spinner-hook
       '(:name "Agent" :args (:subagent_type "explorer"))))
    (with-current-buffer view-buf
      (should-not (mevedel-view--request-progress-visible-p))
      (should-not (string-match-p "Calling Agent"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))

  :doc "Agent pre-tool keeps request progress without duplicate pending line"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-spinner-frames '("-" "+"))
          (mevedel-view--spinner-frame-index 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (mevedel-view--spinner-hook
         '(:name "Agent" :args (:subagent_type "verifier")))
        (mevedel-view--pre-tool-hook
         '(:id "agent-1" :name "Agent"
                :args (:subagent_type "verifier"))))
      (with-current-buffer view-buf
        (should (mevedel-view--request-progress-visible-p))
        (should-not mevedel-view--pending-tool-calls)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Working" text))
          (should-not (string-match-p "Calling Agent" text))))))
)


;;
;;; View buffer setup

(mevedel-deftest mevedel-view--setup ()
  ,test
  (test)
  :doc "wires buffers together correctly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (eq mevedel--data-buffer data-buf))
      (should mevedel-view--input-marker)
      (should (derived-mode-p 'mevedel-view-mode))
      (should-not buffer-read-only))
    (with-current-buffer data-buf
      (should (eq mevedel--view-buffer view-buf))
      (should (eq (local-key-binding (kbd "C-c C-o"))
                  #'mevedel-menu))
      (should-not (eq (local-key-binding (kbd "C-c C-m"))
                      #'mevedel-menu))
      (let ((data-map (current-local-map)))
        (with-temp-buffer
          (org-mode)
          (should-not (eq data-map (current-local-map)))
          (should-not (eq (lookup-key (current-local-map) (kbd "C-c C-o"))
                          #'mevedel-menu))))))

  :doc "view buffers are ephemeral and never offered for saving"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not buffer-auto-save-file-name)
      (goto-char (point-max))
      (insert "draft input")
      (should-not buffer-file-name)
      (should-not buffer-offer-save)
      (should-not (buffer-modified-p))
      (should-not (memq view-buf (files--buffers-needing-to-be-saved t))))
    (let ((prompted nil))
	      (cl-letf (((symbol-function 'read-file-name)
	                 (lambda (&rest _)
	                   (setq prompted t)
	                   (error "View buffer requested save filename")))
	                ((symbol-function 'y-or-n-p)
	                 (lambda (&rest _)
	                   (setq prompted t)
	                   (error "View buffer requested save confirmation"))))
        (save-some-buffers t (lambda () (eq (current-buffer) view-buf))))
      (should-not prompted))))

  :doc "view buffers stay out of save prompts even if a file name leaks in"
  (mevedel-view-test--with-buffers
    (let ((fake-file (make-temp-file "mevedel-view-leaked-file-")))
      (unwind-protect
          (with-current-buffer view-buf
            (setq buffer-file-name fake-file
                  buffer-file-truename (file-truename fake-file))
            (set-buffer-modified-p t)
            (goto-char (point-max))
            (insert "draft input")
            (should-not buffer-file-name)
            (should-not buffer-file-truename)
            (should-not (buffer-modified-p))
            (should-not (memq view-buf
                              (files--buffers-needing-to-be-saved t))))
        (when (file-exists-p fake-file)
          (delete-file fake-file)))))

(mevedel-deftest mevedel-view--status-strip-button ()
  ,test
  (test)
  :doc "status strip button routes clicks to the requested cockpit area"
  (let ((button (mevedel-view--status-strip-button
                 "Mode" 'mode "Open mode cockpit"))
        called)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (area) (setq called area))))
      (let* ((map (get-text-property 0 'local-map button))
             (command (lookup-key map [header-line mouse-1])))
        (should (eq (get-text-property 0 'mevedel-view-cockpit-area button)
                    'mode))
        (should (string= button "Mode"))
        (should command)
        (funcall command nil)
        (should (eq called 'mode))))))

(mevedel-deftest mevedel-view--status-strip ()
  ,test
  (test)
  :doc "status strip root label truncates to the workspace tail, then disappears"
  (let ((root "~/Projekte/mevedel/"))
    (should (equal root
                   (mevedel-view--status-strip-root-label root 24)))
    (should (equal "…/mevedel/"
                   (mevedel-view--status-strip-root-label root 10)))
    (should (equal ""
                   (mevedel-view--status-strip-root-label root 9))))

  :doc "status strip shows mevedel-owned session orientation instead of the data header"
  (let* ((root (make-temp-file "mevedel-status-root-" t))
         (workspace (mevedel-workspace-get-or-create
                     'project (format "status-%s" root) root "mevedel"))
         (session (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-permission-mode session) 'plan)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local default-directory (file-name-as-directory root))
            (setq-local header-line-format "GPTEL HEADER")
            (setq-local mevedel--session session)
            (setq-local gptel-model 'gpt-5.5)
            (setq-local gptel-tools '(read edit)))
          (with-current-buffer view-buf
            (let ((line (mevedel-view--status-strip)))
              (should (string-prefix-p "main  " line))
              (should (string-match-p
                       (regexp-quote
                        (file-name-nondirectory
                         (directory-file-name root)))
                       line))
              (should (string-match-p
                       (regexp-quote "plan · idle · gpt-5.5 · 2 tools")
                       line))
              (should-not (string-match-p "mevedel:" line))
              (should-not (string-match-p "\\[gpt-5\\.5\\]" line))
              (should-not (string-match-p "\\[2 tools\\]" line))
              (should-not (string-match-p "GPTEL HEADER" line)))))
	  (when (file-directory-p root)
	    (delete-directory root t))))

  :doc "status strip preserves the model none label"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip)))
        (should (string-match-p
                 (regexp-quote "ask · idle · model none · 0 tools")
                 line)))))

  :doc "status strip routes click targets to cockpit surfaces"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((line (mevedel-view--status-strip))
            called)
        (cl-letf (((symbol-function 'mevedel-menu-open)
                   (lambda (area) (setq called area))))
          (dolist (area '(top mode model tools))
            (let* ((pos (text-property-any
                         0 (length line)
                         'mevedel-view-cockpit-area area line))
                   (map (and pos (get-text-property pos 'local-map line)))
                   (command (and map
                                 (lookup-key map [header-line mouse-1]))))
              (should pos)
              (should command)
              (setq called nil)
              (funcall command nil)
              (should (eq called area))))))))

  :doc "status strip clicks do not call gptel transients directly"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((line (mevedel-view--status-strip))
             (pos (text-property-any
                   0 (length line)
                   'mevedel-view-cockpit-area 'tools line))
             (map (get-text-property pos 'local-map line))
             (command (lookup-key map [header-line mouse-1]))
             (gptel-called nil))
        (cl-letf (((symbol-function 'gptel-menu)
                   (lambda ()
                     (interactive)
                     (setq gptel-called t)))
                  ((symbol-function 'mevedel-menu-open) #'ignore))
          (funcall command nil)
          (should-not gptel-called)))))

  :doc "status strip keeps the raw data buffer header line"
  (let ((data-buf (generate-new-buffer " *status-data*"))
        (view-buf (generate-new-buffer " *status-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local header-line-format "GPTEL HEADER"))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer data-buf
            (should (equal header-line-format "GPTEL HEADER")))
          (with-current-buffer view-buf
            (should (equal header-line-format
                           '(:eval (mevedel-view--status-strip))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--dnd-file-mentions
  (:doc "view drag/drop inserts @file mentions and records exact grants")
  ,test
  (test)
  (let* ((dir (make-temp-file "mevedel dnd-" t))
         (path (expand-file-name "image file.png" dir))
         (data-buf (generate-new-buffer " *test-data-dnd*"))
         (view-buf (generate-new-buffer " *test-view-dnd*"))
         (ws (mevedel-workspace--create :type 'project :id "dnd"
                                        :root dir :name "dnd"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-file path (insert "fake image\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (mevedel-view--insert-dropped-file-mentions (list path))
            (should (equal (format "@file:{%s}" path)
                           (mevedel-view--input-text)))
            (should (equal (list path)
                           (mevedel-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "DND handler accepts the single-URI protocol-handler shape"
  (let* ((dir (make-temp-file "mevedel-dnd-" t))
         (path (expand-file-name "single.txt" dir))
         (uri (concat "file://" path))
         (data-buf (generate-new-buffer " *test-data-dnd-single*"))
         (view-buf (generate-new-buffer " *test-view-dnd-single*"))
         (ws (mevedel-workspace--create :type 'project :id "dnd-single"
                                        :root dir :name "dnd-single"))
         (session (mevedel-session-create "main" ws)))
    (with-temp-file path (insert "single\n"))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (should (eq 'copy
                        (mevedel-view--dnd-handle-files uri 'copy)))
            (should (equal (format "@file:%s" path)
                           (mevedel-view--input-text)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "yank-dwim saves clipboard images to workspace media and inserts @file"
  (let* ((dir (make-temp-file "mevedel-clipboard-" t))
         (data-buf (generate-new-buffer " *test-data-clipboard*"))
         (view-buf (generate-new-buffer " *test-view-clipboard*"))
         (ws (mevedel-workspace--create :type 'project :id "clipboard"
                                        :root dir :name "clipboard"))
         (session (mevedel-session-create "main" ws))
         (expected (file-name-concat
                    dir ".mevedel" "media"
                    "clipboard-20260620-121314.png"))
         (mevedel-view-clipboard-image-handlers
          `(((:command . "fake-clipboard")
             (:save . ,(lambda (file-path)
                         (with-temp-file file-path
                           (set-buffer-multibyte nil)
                           (insert "png"))))))))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local gptel-response-separator "\n\n")
            (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace ws))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (goto-char (point-max))
            (cl-letf (((symbol-function 'window-system)
                       (lambda (&optional _frame) 'x))
                      ((symbol-function 'executable-find)
                       (lambda (command)
                         (and (equal command "fake-clipboard")
                              command)))
                      ((symbol-function 'format-time-string)
                       (lambda (&rest _) "20260620-121314")))
              (mevedel-view-yank-dwim))
            (should (file-exists-p expected))
            (should (equal (format "@file:%s" expected)
                           (mevedel-view--input-text)))
            (should (equal (list expected)
                           (mevedel-session-dropped-file-grants session)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory dir t)))

  :doc "yank-dwim falls back to text yank when image saving has no session"
  (with-temp-buffer
    (let ((kill-ring '("plain text"))
          (kill-ring-yank-pointer nil)
          (mevedel-view-clipboard-image-handlers
           '(((:command . "fake-clipboard")
              (:save . ignore)))))
      (cl-letf (((symbol-function 'window-system)
                 (lambda (&optional _frame) 'x))
                ((symbol-function 'executable-find)
                 (lambda (command)
                   (and (equal command "fake-clipboard")
                        command))))
        (mevedel-view-yank-dwim))
      (should (equal "plain text" (buffer-string))))))


;;
;;; gptel stream and bridge helpers

(mevedel-deftest mevedel-view--repair-gptel-stream-info ()
  ,test
  (test)
  :doc "reanchors detached position markers for mevedel stream callbacks"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "Prompt\n"))
    (let ((position (copy-marker 1 nil))
          (tracking (copy-marker 1 nil))
          (reasoning (copy-marker 1 nil)))
      (set-marker position nil)
      (set-marker tracking nil)
      (set-marker reasoning nil)
      (let ((info (list :buffer data-buf
                        :position position
                        :tracking-marker tracking
                        :reasoning-marker reasoning)))
        (should (eq (mevedel-view--repair-gptel-stream-info info) info))
        (let ((new-position (plist-get info :position)))
          (should (markerp new-position))
          (should (eq (marker-buffer new-position) data-buf))
          (should (= (marker-position new-position)
                     (with-current-buffer data-buf (point-max)))))
        (should-not (plist-get info :tracking-marker))
        (should-not (plist-get info :reasoning-marker)))))

  :doc "leaves unrelated gptel stream info untouched"
  (with-temp-buffer
    (let ((position (copy-marker (point) nil)))
      (set-marker position nil)
      (let ((info (list :buffer (current-buffer)
                        :position position)))
        (mevedel-view--repair-gptel-stream-info info)
        (should (eq (plist-get info :position) position))
        (should-not (marker-position position))))))

(mevedel-deftest mevedel-view--gptel-stream-insert-response-advice ()
  ,test
  (test)
  :doc "repairs detached markers before delegating to gptel streaming"
  (mevedel-view-test--with-buffers
    (let ((position (copy-marker 1 nil))
          (mevedel-view-stream-insert-batch-delay nil))
      (set-marker position nil)
      (let ((info (list :buffer data-buf :position position)))
        (should
         (eq (mevedel-view--gptel-stream-insert-response-advice
              (lambda (_response callback-info &optional _raw)
                (let ((marker (plist-get callback-info :position)))
                  (and (markerp marker)
                       (eq (marker-buffer marker) data-buf)
                       (marker-position marker)
                       'delegated)))
              "chunk"
             info)
             'delegated))))))

(mevedel-deftest mevedel-view--gptel-stream-insert-response-advice/performance ()
  ,test
  (test)

  :doc "mevedel stream insertion suppresses after-change hooks but keeps post-stream hook"
  (mevedel-view-test--with-buffers
    (let ((after-change-ran nil)
          (post-stream-ran nil)
          (mevedel-view-stream-insert-batch-delay nil)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (with-current-buffer data-buf
        (setq-local after-change-functions nil)
        (setq-local gptel-post-stream-hook nil)
        (add-hook 'after-change-functions
                  (lambda (&rest _) (setq after-change-ran t))
                  nil t)
        (add-hook 'gptel-post-stream-hook
                  (lambda () (setq post-stream-ran t))
                  nil t))
      (mevedel-view--gptel-stream-insert-response-advice
       (lambda (response callback-info &optional _raw)
         (with-current-buffer (plist-get callback-info :buffer)
           (goto-char (plist-get callback-info :position))
           (insert response)
           (run-hooks 'gptel-post-stream-hook)))
       "chunk" info)
      (should-not after-change-ran)
      (should post-stream-ran)
      (with-current-buffer data-buf
        (should (string-match-p "chunk" (buffer-string))))))

  :doc "batching coalesces consecutive string stream chunks"
  (mevedel-view-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig "a" info)
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig "b" info)
        (should-not calls)
        (mevedel-view--flush-gptel-stream-insert-batch info)
        (should (equal '(("ab" nil)) calls)))))

  :doc "batching flushes pending strings before non-string events"
  (mevedel-view-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig "text" info)
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig '(tool-call . nil) info)
        (should (equal '(((tool-call) nil) ("text" nil)) calls)))))

  :doc "batching flushes when raw insertion mode changes"
  (mevedel-view-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig "raw" info t)
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig "normal" info nil)
        (mevedel-view--flush-gptel-stream-insert-batch info)
        (should (equal '(("normal" nil) ("raw" t)) calls)))))

  :doc "batching flushes before stream cleanup"
  (mevedel-view-test--with-buffers
    (let* ((process (make-pipe-process
                     :name "mevedel-test-stream-cleanup"))
           (calls nil)
           (mevedel-view-stream-insert-batch-delay 10)
           (info (list :buffer data-buf
                       :position (with-current-buffer data-buf
                                   (copy-marker (point-max) nil))))
           (fsm (gptel-make-fsm :info info))
           (gptel--request-alist (list (cons process (cons fsm #'ignore)))))
      (unwind-protect
          (cl-labels ((orig (response _info &optional raw)
                        (push (list response raw) calls)))
            (mevedel-view--gptel-stream-insert-response-advice
             #'orig "a" info)
            (mevedel-view--gptel-stream-insert-response-advice
             #'orig "b" info)
            (mevedel-view--gptel-stream-cleanup-advice
             (lambda (_process status)
               (push (list 'cleanup status) calls))
             process "finished")
            (should (equal '((cleanup "finished") ("ab" nil)) calls)))
        (when (process-live-p process)
          (delete-process process)))))

  :doc "batching drops stale batches after stream buffer teardown"
  (mevedel-view-test--with-buffers
    (let ((calls nil)
          (mevedel-view-stream-insert-batch-delay 10)
          (info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil)))))
      (cl-labels ((orig (response _info &optional raw)
                    (push (list response raw) calls)))
        (mevedel-view--gptel-stream-insert-response-advice
         #'orig "stale" info)
        (kill-buffer data-buf)
        (mevedel-view--flush-gptel-stream-insert-batch info)
        (should-not calls))))

  :doc "nil or zero batch delay preserves immediate insertion"
  (dolist (delay '(nil 0))
    (mevedel-view-test--with-buffers
      (let ((calls nil)
            (mevedel-view-stream-insert-batch-delay delay)
            (info (list :buffer data-buf
                        :position (with-current-buffer data-buf
                                    (copy-marker (point-max) nil)))))
        (cl-labels ((orig (response _info &optional raw)
                      (push (list response raw) calls)))
          (mevedel-view--gptel-stream-insert-response-advice
           #'orig "a" info)
          (mevedel-view--gptel-stream-insert-response-advice
           #'orig "b" info)
          (should (equal '(("b" nil) ("a" nil)) calls)))))))

(mevedel-deftest mevedel-view--wrap-gptel-stream-transformer ()
  ,test
  (test)
  :doc "stale gptel stream transformer errors return the raw chunk"
  (mevedel-view-test--with-buffers
    (let ((info (list :buffer data-buf
                      :position (with-current-buffer data-buf
                                  (copy-marker (point-max) nil))
                      :transformer
                      (lambda (_str)
                        (error "Selecting deleted buffer")))))
      (mevedel-view--repair-gptel-stream-info info)
      (should (plist-get info :mevedel-transformer-wrapped))
      (should (equal "raw chunk"
                     (funcall (plist-get info :transformer)
                              "raw chunk"))))))

(mevedel-deftest mevedel-view--gptel-stream-filter-advice ()
  ,test
  (test)
  :doc "buffers stream chunks until gptel registers the process FSM"
  (let ((process (make-pipe-process
                  :name "mevedel-test-stream-filter"))
        (gptel--request-alist nil)
        (calls nil)
        scheduled)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-view--schedule-gptel-stream-filter-flush)
                   (lambda (proc) (setq scheduled proc))))
          (mevedel-view--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: a\n")
          (should (eq scheduled process))
          (should-not calls)
          (should (equal "event: a\n"
                         (process-get
                          process 'mevedel-view--pending-stream-output)))
          (setf (alist-get process gptel--request-alist)
                (cons 'fake-fsm #'ignore))
          (mevedel-view--gptel-stream-filter-advice
           (lambda (_process output)
             (push output calls))
           process "event: b\n")
          (should (equal '("event: a\nevent: b\n") calls))
          (should-not
           (process-get process 'mevedel-view--pending-stream-output)))
      (when (process-live-p process)
        (delete-process process)))))

(mevedel-deftest mevedel-view--install-gptel-stream-advice-if-enabled ()
  ,test
  (test)
  :doc "deferred installer is a no-op after uninstall disables stream repair"
  (let ((calls 0)
        (mevedel-view--gptel-stream-advice-installed nil))
    (cl-letf (((symbol-function 'mevedel-view--install-gptel-stream-advice)
               (lambda () (cl-incf calls))))
      (mevedel-view--install-gptel-stream-advice-if-enabled)
      (should (= calls 0))
      (let ((mevedel-view--gptel-stream-advice-installed t))
        (mevedel-view--install-gptel-stream-advice-if-enabled))
      (should (= calls 1)))))

(mevedel-deftest mevedel-view-uninstall-gptel-stream-advice ()
  ,test
  (test)
  :doc "uninstall disables future deferred installs"
  (let ((mevedel-view--gptel-stream-advice-installed t))
    (cl-letf (((symbol-function 'mevedel-view--uninstall-gptel-stream-advice)
               #'ignore))
      (mevedel-view-uninstall-gptel-stream-advice))
    (should-not mevedel-view--gptel-stream-advice-installed)))

(mevedel-deftest mevedel-view--on-view-killed
  (:doc "view kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the view aborts both queues and kills the data buffer"
  (let ((data-buf (generate-new-buffer " *test-data-kill-view*"))
        (view-buf (generate-new-buffer " *test-view-kill-view*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-view/"
                   :root "/tmp/kill-view/" :name "kill-view")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-plan-queue session)
                (list (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes)))))
          (kill-buffer view-buf)
          (should-not (buffer-live-p view-buf))
          (should-not (buffer-live-p data-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-plan-queue session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--on-data-killed
  (:doc "data kill hook cleans up queued interactions")
  ,test
  (test)

  :doc "killing the data buffer aborts both queues and kills the view"
  (let ((data-buf (generate-new-buffer " *test-data-kill-data*"))
        (view-buf (generate-new-buffer " *test-view-kill-data*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/kill-data/"
                   :root "/tmp/kill-data/" :name "kill-data")))
        (outcomes nil))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'permission outcome) outcomes)))))
          (setf (mevedel-session-plan-queue session)
                (list (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push (cons 'plan outcome) outcomes)))))
          (kill-buffer data-buf)
          (should-not (buffer-live-p data-buf))
          (should-not (buffer-live-p view-buf))
          (should (null (mevedel-session-permission-queue session)))
          (should (null (mevedel-session-plan-queue session)))
          (should (equal '((plan . aborted) (permission . aborted))
                         outcomes)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--interaction-target-buffer
  (:doc "resolves the live parent view for queued interactions")
  ,test
  (test)

  :doc "agent data buffers fall back through invocation parent data"
  (let ((parent-data (generate-new-buffer " *test-parent-data-prompt*"))
        (parent-view (generate-new-buffer " *test-parent-view-prompt*"))
        (agent-data (generate-new-buffer " *test-agent-data-prompt*"))
        (session (mevedel-session-create
                  "main"
                  (mevedel-workspace--create
                   :type 'project :id "/tmp/prompt-view/"
                   :root "/tmp/prompt-view/" :name "prompt-view"))))
    (unwind-protect
        (let ((inv (mevedel-agent-invocation--create
                    :agent-id "verifier--prompt123"
                    :parent-data-buffer parent-data
                    :buffer agent-data
                    :transcript-status 'running)))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation inv)
            (setq-local mevedel--view-buffer nil))
          (should (eq parent-view
                      (mevedel-view--interaction-target-buffer
                       agent-data)))
          (with-current-buffer agent-data
            (should (eq parent-view
                        (mevedel-view--interaction-target-buffer)))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data)))))


;;
;;; Input forwarding

(mevedel-deftest mevedel-view--input-prompt-string
  (:doc "renders permission mode in the prompt prefix")
  ,test
  (test)

  :doc "default mode renders ask"
  (let ((prompt (mevedel-view--input-prompt-string 'default)))
    (should (string= "\n> " prompt))
    (should (eq 'mevedel-view-input-prompt
                (get-text-property 0 'font-lock-face prompt))))

  :doc "plan mode renders plan"
  (let ((prompt (mevedel-view--input-prompt-string 'plan)))
    (should (string= "\n[plan]  > " prompt))
    (should (eq 'mevedel-view-permission-mode-plan
                (get-text-property 2 'font-lock-face prompt))))

  :doc "accept-edits mode renders edits"
  (let ((prompt (mevedel-view--input-prompt-string 'accept-edits)))
    (should (string= "\n[edits] > " prompt))
    (should (eq 'mevedel-view-permission-mode-accept-edits
                (get-text-property 2 'font-lock-face prompt))))

  :doc "trust-all mode renders auto warning"
  (let ((prompt (mevedel-view--input-prompt-string 'trust-all)))
    (should (string= "\n[auto!] > " prompt))
    (should (eq 'mevedel-view-permission-mode-trust-all
                (get-text-property 2 'font-lock-face prompt)))))

(mevedel-deftest mevedel-view--next-permission-mode
  (:doc "cycles permission modes in view order")
  ,test
  (test)

  :doc "default mode moves to accept-edits"
  (should (eq 'accept-edits
              (mevedel-view--next-permission-mode 'default)))

  :doc "accept-edits mode moves to trust-all"
  (should (eq 'trust-all
              (mevedel-view--next-permission-mode 'accept-edits)))

  :doc "trust-all mode moves to plan"
  (should (eq 'plan
              (mevedel-view--next-permission-mode 'trust-all)))

  :doc "plan mode wraps to default"
  (should (eq 'default
              (mevedel-view--next-permission-mode 'plan)))

  :doc "nil mode starts at accept-edits"
  (should (eq 'accept-edits
              (mevedel-view--next-permission-mode nil)))

  :doc "unknown mode starts at accept-edits"
  (should (eq 'accept-edits
              (mevedel-view--next-permission-mode 'bogus))))

(mevedel-deftest mevedel-view-cycle-permission-mode
  (:doc "cycles the current session mode and refreshes the prompt")
  ,test
  (test)

  :doc "cycles session mode and refreshes prompt"
  (let ((saved (default-toplevel-value 'mevedel-permission-mode)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (let ((session (mevedel-session--create
                          :name "main"
                          :permission-mode 'default)))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--view-buffer view-buf)
              (setq-local mevedel-permission-mode 'default))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-permission-mode 'default)
              (should (eq 'accept-edits
                          (mevedel-view-cycle-permission-mode)))
              (should (eq 'accept-edits
                          (mevedel-session-permission-mode session)))
              (should (eq 'accept-edits
                          (buffer-local-value
                           'mevedel-permission-mode data-buf)))
              (should (eq 'accept-edits mevedel-permission-mode))
              (should (eq saved
                          (default-toplevel-value 'mevedel-permission-mode)))
              (should (string= "\n[edits] > "
                               (buffer-substring-no-properties
                                mevedel-view--input-marker
                                (mevedel-view--input-start)))))
            (with-current-buffer view-buf
              (should (eq 'trust-all
                          (mevedel-view-cycle-permission-mode)))
              (should (memq 'auto-mode
                            (mapcar #'mevedel-reminder-type
                                    (mevedel-session-reminders session))))
              (should (eq 'plan
                          (mevedel-view-cycle-permission-mode)))
              (let ((types (mapcar #'mevedel-reminder-type
                                   (mevedel-session-reminders session))))
                (should (eq 'trust-all
                            (plist-get
                             (mevedel-session-plan-metadata session)
                             :previous-permission-mode)))
                (should (memq 'plan-mode types))
                (should-not (memq 'auto-mode types))
                (should (memq 'auto-mode-exit types)))
              (should (eq 'default
                          (mevedel-view-cycle-permission-mode)))
              (let ((types (mapcar #'mevedel-reminder-type
                                   (mevedel-session-reminders session))))
                (should-not
                 (plist-get (mevedel-session-plan-metadata session)
                            :previous-permission-mode))
                (should-not (memq 'plan-mode types))
                (should (memq 'plan-mode-exit types)))
              (should (eq 'default
                          (mevedel-session-permission-mode session))))))
      (set-default-toplevel-value 'mevedel-permission-mode saved))))

(mevedel-deftest mevedel-view-cycle-permission-mode-keymap
  (:doc "binds backtab spellings to permission mode cycling")
  ,test
  (test)

  :doc "backtab is bound"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "<backtab>"))
              #'mevedel-view-cycle-permission-mode))

  :doc "S-TAB is bound"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "S-TAB"))
              #'mevedel-view-cycle-permission-mode)))

(mevedel-deftest mevedel-view-mode-map ()
  ,test
  (test)
  :doc "view mode binds the cockpit command"
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-o"))
              #'mevedel-menu))
  (should-not (eq (lookup-key mevedel-view-mode-map (kbd "C-c C-m"))
                  #'mevedel-menu))
  (should (eq (lookup-key mevedel-view-mode-map (kbd "C-c RET"))
              #'mevedel-view-send)))

(mevedel-deftest mevedel-view-user-query-navigation ()
  ,test
  (test)
  :doc "jumps between visible user query headers"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let (first second)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--status-marker)
          (setq first (point))
          (insert (propertize "You\n"
                              'mevedel-view-turn-role 'user))
          (insert "first question\n")
          (insert (propertize "Assistant\n"
                              'mevedel-view-turn-role 'assistant))
          (insert "answer\n")
          (setq second (point))
          (insert (propertize "You\n"
                              'mevedel-view-turn-role 'user))
          (insert "second question\n")
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point)))
        (goto-char (point-min))
        (mevedel-view-next-user-query)
        (should (= (point) first))
        (mevedel-view-next-user-query)
        (should (= (point) second))
        (mevedel-view-next-user-query)
        (should (= (point) second))
        (goto-char (+ second (length "You\ns")))
        (mevedel-view-previous-user-query)
        (should (= (point) second))
        (mevedel-view-previous-user-query)
        (should (= (point) first))))))

(mevedel-deftest mevedel-view-refresh-input-prompt
  (:doc "updates the prompt prefix without disturbing draft input")
  ,test
  (test)

  :doc "setup renders the default mode prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (string= "\n> "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))))

  :doc "refresh preserves input text and updates the mode"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (setq-local mevedel-permission-mode 'trust-all)
      (mevedel-view-refresh-input-prompt)
      (should (string= "\n[auto!] > "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))
      (should (string= "draft" (mevedel-view--input-text)))))

  :doc "refresh preserves a multiline draft starting with a literal >"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "> quoted\nsecond line")
      (setq-local mevedel-permission-mode 'trust-all)
      (mevedel-view-refresh-input-prompt)
      (should (string= "\n[auto!] > "
                       (buffer-substring-no-properties
                        mevedel-view--input-marker
                        (mevedel-view--input-start))))
      (should (string= "> quoted\nsecond line" (mevedel-view--input-text)))
      (should-not (get-text-property (mevedel-view--input-start)
                                     'mevedel-view-prompt))))

  :doc "refresh recovers drifted markers before updating the prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (setq-local mevedel-permission-mode 'trust-all)
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (should (string= "\n[auto!] > "
                         (buffer-substring-no-properties
                          mevedel-view--input-marker
                          (mevedel-view--input-start))))))))

(mevedel-deftest mevedel-view--call-preserving-input-text
  (:doc "restores composer text after accidental render insertion")
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (mevedel-view--call-preserving-input-text
       (lambda ()
         (goto-char (point-max))
         (insert "\nagent result leaked into composer")))
      (should (string= "draft" (mevedel-view--input-text)))
      (let ((changes 0))
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--call-preserving-input-text
         (lambda ()
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (insert "status row\n"))))
        (should (= 0 changes))))))

(mevedel-deftest mevedel-view--input-text ()
  ,test
  (test)
  :doc "extracts text from input region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (string-empty-p (mevedel-view--input-text)))
      (goto-char (mevedel-view--input-start))
      (insert "hello world")
      (should (equal "hello world" (mevedel-view--input-text)))))

  :doc "clear empties input region"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "hello world")
      (mevedel-view--clear-input)
      (should (string-empty-p (mevedel-view--input-text))))))

(mevedel-deftest mevedel-view-slash-capf
  (:before-each
   (progn
     (clrhash mevedel-skills--dir-buffers)
     (clrhash mevedel-skills--dirty-buffers)
     (clrhash mevedel-skills--mtime-cache))
   :after-each
   (progn
     (clrhash mevedel-skills--dir-buffers)
     (clrhash mevedel-skills--dirty-buffers)
     (clrhash mevedel-skills--mtime-cache)))
  ,test
  (test)
  :doc "view skill completion refreshes after skill saves"
  (let* ((mevedel-skills-include-bundled nil)
         (mevedel-skills-check-for-modifications '(check-on-save))
         (root (make-temp-file "mevedel-view-skills-" t))
         (mevedel-skill-dirs (list root))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-skills"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (mevedel-view-test--write-skill
           root "alpha" "name: alpha\ndescription: A\n")
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (mevedel-skills-install session data-buf))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$")
            (let ((capf (mevedel-view-slash-capf)))
              (should (member "alpha"
                              (mevedel-view-test--capf-candidates capf)))
              (let ((skill-file
                     (mevedel-view-test--write-skill
                      root "bar" "name: bar\ndescription: B\n")))
                (with-temp-buffer
                  (setq buffer-file-name skill-file)
                  (mevedel-skills--before-save-hook)))
	              (should (member "bar"
	                              (mevedel-view-test--capf-candidates
	                               capf "b"))))))
	      (delete-directory root t)))

  :doc "view mode command completes first argument options"
  (let* ((root (make-temp-file "mevedel-view-mode-capf-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-mode-capf"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "/mode pl")
            (let ((capf (mevedel-view-slash-capf)))
              (should capf)
              (should (equal '("plan")
                             (mevedel-view-test--capf-candidates
                              capf "pl")))
              (should (member "trust-all"
                              (mevedel-view-test--capf-candidates capf))))))
      (delete-directory root t)))

  :doc "view review command completes target arguments"
  (let* ((root (make-temp-file "mevedel-view-review-capf-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-review-capf"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "/review cur")
            (let ((capf (mevedel-view-slash-capf)))
              (should capf)
              (should (equal '("current")
                             (mevedel-view-test--capf-candidates
                              capf "cur"))))))
      (delete-directory root t)))

  :doc "view root completion inserts a real separator before skill hint"
  (let* ((root (make-temp-file "mevedel-view-root-space-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-root-space"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "remember"
                 :argument-names '("focus"))))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$rem")
            (let* ((capf (mevedel-view-slash-capf))
                   (exit (and capf (plist-get (nthcdr 3 capf)
                                              :exit-function))))
              (delete-region (nth 0 capf) (nth 1 capf))
              (insert "remember")
              (funcall exit "remember" 'finished)
              (mevedel-view--refresh-skill-argument-hint)
              (should (string-match-p
                       "\\[focus\\]"
                       (mevedel-view-test--skill-hint-string)))
              (insert "d")
              (should (equal "$remember d"
                             (mevedel-view--input-text))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view--refresh-skill-argument-hint ()
  ,test
  (test)
  :doc "argument-hint appears as overlay text before args"
  (let* ((root (make-temp-file "mevedel-view-skill-hint-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-skill-hint"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "green-loop"
                 :argument-hint "What change should be validated?")))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$green-loop")
            (mevedel-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "What change should be validated"
                     (mevedel-view-test--skill-hint-string)))
            (should (equal "$green-loop" (mevedel-view--input-text)))
            (insert " current changes")
            (mevedel-view--refresh-skill-argument-hint)
            (should-not (mevedel-view-test--skill-hint-string))))
      (delete-directory root t)))

  :doc "argument names show only remaining slots"
  (let* ((root (make-temp-file "mevedel-view-named-hint-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-named-hint"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "deploy-api"
                 :argument-names '("service" "environment"))))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$deploy-api")
            (mevedel-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "\\[service\\] \\[environment\\]"
                     (mevedel-view-test--skill-hint-string)))
            (insert " billing")
            (mevedel-view--refresh-skill-argument-hint)
            (should (string-match-p
                     "\\[environment\\]"
                     (mevedel-view-test--skill-hint-string)))
            (should-not (string-match-p
                         "\\[service\\]"
                         (mevedel-view-test--skill-hint-string)))))
      (delete-directory root t)))

  :doc "non-skill input clears the overlay"
  (let* ((root (make-temp-file "mevedel-view-clear-hint-" t))
         (ws (mevedel-workspace--create
              :type 'test :id root :root root :name "view-clear-hint"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "green-loop"
                 :argument-hint "What change should be validated?")))
    (setf (mevedel-session-skills session) (list skill))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session))
          (with-current-buffer view-buf
            (goto-char (mevedel-view--input-start))
            (insert "$green-loop")
            (mevedel-view--refresh-skill-argument-hint)
            (should (mevedel-view-test--skill-hint-string))
            (mevedel-view--clear-input)
            (insert "hello")
            (mevedel-view--refresh-skill-argument-hint)
            (should-not (mevedel-view-test--skill-hint-string))))
      (delete-directory root t)))

  :doc "history-region refresh skips prompt scan"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (goto-char (point-min))
      (cl-letf (((symbol-function 'mevedel-view--input-start)
                 (lambda ()
                   (error "Prompt scan should be skipped"))))
        (mevedel-view--refresh-skill-argument-hint)
        (should-not mevedel-view--skill-argument-hint-overlay))))

  :doc "refresh repairs drifted input marker"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((prompt-start (mevedel-view--prompt-start-position))
            (input-start (mevedel-view--input-start)))
        (goto-char input-start)
        (insert "draft")
        (set-marker mevedel-view--input-marker (point-max))
        (goto-char input-start)
        (mevedel-view--refresh-skill-argument-hint)
        (should (= prompt-start
                   (marker-position mevedel-view--input-marker)))))))


;;
;;; Full re-render

(mevedel-deftest mevedel-view--fontify-response ()
  ,test
  (test)
  :doc "preserves Markdown response text instead of converting it to Org"
  (let ((text (mevedel-view--fontify-response
               "I’ll inspect `mevedel-review.el` now.\n\n```r\neval(f[[3]], df)\n```")))
    (should (string-match-p "mevedel-review\\.el" text))
    (should (string-match-p "```r" text))
    (should-not (string-match-p "#\\+begin_src" text)))

  :doc "preserves bracket indexing inside fenced code blocks"
  (let ((text (mevedel-view--fontify-response
               "```r\neval(f[[3]], df)\n```")))
    (should (string-match-p "```r" text))
    (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
    (should-not (string-match-p "eval(f3, df)" text))
    (let ((pos (string-match "\\[\\[3\\]\\]" text)))
      (should pos)
      (should-not (get-text-property pos 'htmlize-link text))
      (should-not (get-text-property pos 'help-echo text))
      (should-not (get-text-property pos 'keymap text))
      (should-not (get-text-property pos 'mouse-face text))))

  :doc "removes source-block link properties after affiliated keywords"
  (let ((text (mevedel-view--fontify-response
               "#+NAME: rhs\n#+begin_src r\neval(f[[3]], df)\n#+end_src")))
    (should (string-match-p "#\\+NAME: rhs" text))
    (should (string-match-p "eval(f\\[\\[3\\]\\], df)" text))
    (let ((pos (string-match "\\[\\[3\\]\\]" text)))
      (should pos)
      (should-not (get-text-property pos 'htmlize-link text))
      (should-not (get-text-property pos 'help-echo text))
      (should-not (get-text-property pos 'keymap text))
      (should-not (get-text-property pos 'mouse-face text))))

  :doc "preserves bracket indexing inside inline code"
  (let ((text (mevedel-view--fontify-response
               "Use `f[[3]]` for the right-hand side.")))
    (should (string-match-p "f\\[\\[3\\]\\]" text))
    (should-not (string-match-p "f3" text)))

  :doc "still displays descriptive prose links"
  (let ((text (mevedel-view--fontify-response
               "See [site](https://example.com) and `items[[3]]`.")))
    (should (string-match-p "\\[site\\](https://example\\.com)" text))
    (should (string-match-p "items\\[\\[3\\]\\]" text)))

  :doc "caches repeated response fontification in view buffers"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((calls 0))
        (cl-letf (((symbol-function 'mevedel-view--markdown-fontify-mode)
                   (lambda () 'mevedel-view-test-markdown-mode))
                  ((symbol-function 'mevedel-view-test-markdown-mode)
                   (lambda ()
                     (cl-incf calls)
                     (fundamental-mode))))
          (should (string-match-p "cached"
                                  (mevedel-view--fontify-response "cached")))
          (should (string-match-p "cached"
                                  (mevedel-view--fontify-response "cached")))
          (should (= 1 calls))))))

  :doc "suppresses arbitrary major-mode hooks in render temp buffers"
  (let* ((called nil)
         (hook (lambda ()
                 (setq called t))))
    (unwind-protect
        (progn
          (add-hook 'emacs-lisp-mode-hook hook)
          (mevedel-view--with-render-temp-buffer
            (emacs-lisp-mode))
          (should-not called))
      (remove-hook 'emacs-lisp-mode-hook hook))))

(mevedel-deftest mevedel-view--decorate-code-blocks-in-range
  (:doc "`mevedel-view--decorate-code-blocks-in-range' adds copy buttons to fenced blocks")
  ,test
  (test)
  (with-temp-buffer
    (insert "before\n```elisp\n(+ 1 2)\n```\nafter\n")
    (mevedel-view--decorate-code-blocks-in-range (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "```elisp")
    (let ((button (button-at (match-beginning 0)))
          copied)
      (should button)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _replace)
                   (setq copied text))))
        (button-activate button))
      (should (equal "(+ 1 2)\n" copied)))))

(mevedel-deftest mevedel-view--decorate-markdown-url-links-in-range
  (:doc "`mevedel-view--decorate-markdown-in-range' renders Markdown links")
  ,test
  (test)
  (with-temp-buffer
    (insert "[Engineer](http://x.com)\n")
    (add-text-properties (point-min) (point-max)
                         '(keymap stale-map
                           follow-link t
                           help-echo "stale markdown link"))
    (mevedel-view--decorate-markdown-in-range (point-min) (point-max))
    (should (equal "Engineer\n" (buffer-string)))
    (goto-char (point-min))
    (search-forward "Engineer")
    (let ((button (button-at (match-beginning 0))))
      (should button)
      (should (equal "http://x.com"
                     (button-get button 'mevedel-view-url))))))

(mevedel-deftest mevedel-view--decorate-local-images-in-range
  (:doc "`mevedel-view--decorate-local-images-in-range' displays local image references")
  ,test
  (test)
  :doc "renders Markdown image links"
  (let ((file (make-temp-file "mevedel-image-link-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "![shot](%s)\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (mevedel-view--decorate-local-images-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward "![shot]")
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (should (equal (list 'image :file file) display))))
      (delete-file file)))

  :doc "renders bare local image paths"
  (let ((file (make-temp-file "mevedel-image-bare-" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (insert (format "Image: %s\n" file))
          (cl-letf (((symbol-function 'display-images-p)
                     (lambda (&optional _display) t))
                    ((symbol-function 'create-image)
                     (lambda (path &rest _)
                       (list 'image :file path))))
            (mevedel-view--decorate-local-images-in-range
             (point-min) (point-max)))
          (goto-char (point-min))
          (search-forward file)
          (let ((display (get-text-property (match-beginning 0) 'display)))
            (should (equal (list 'image :file file) display))))
      (delete-file file))))

(mevedel-deftest mevedel-view--prettify-markdown-tables-in-range
  (:doc "`mevedel-view--prettify-markdown-tables-in-range' aligns pipe tables")
  ,test
  (test)
  :doc "pads cells and separator rows"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| Alice | Engineer |\n")
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should (equal "| Name  | Role     |\n|-------|----------|\n| Alice | Engineer |\n"
                   (buffer-string))))

  :doc "uses visible Markdown text width for simple emphasis and links"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| **Alice** | [Engineer](http://x.com) |\n")
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should (equal "| Name  | Role     |\n|-------|----------|\n| **Alice** | [Engineer](http://x.com) |\n"
                   (buffer-string))))

  :doc "skips tables inside fenced code blocks"
  (let ((text "```md\n| A | B |\n|---|---|\n| x | yy |\n```\n"))
    (with-temp-buffer
      (insert text)
      (mevedel-view--prettify-markdown-tables-in-range
       (point-min) (point-max))
      (should (equal text (buffer-string)))))

  :doc "preserves caller point"
  (with-temp-buffer
    (insert "| Name | Role |\n")
    (insert "|------|------|\n")
    (insert "| Alice | Engineer |\n")
    (goto-char (point-max))
    (mevedel-view--prettify-markdown-tables-in-range
     (point-min) (point-max))
    (should (= (point) (point-max)))))

(mevedel-deftest mevedel-view--live-tail-lines-rendered-position ()
  ,test
  (test)

  :doc "finds live-tail lines separated by blank gaps"
  (with-temp-buffer
    (insert "before\nalpha\n\n   beta\nafter\n")
    (should (= 8 (mevedel-view--live-tail-lines-rendered-position
                  '("alpha" "beta") (point-max)))))

  :doc "does not build an overflowing regexp for long unmatched tails"
  (with-temp-buffer
    (dotimes (i 3000)
      (insert (format "line-%04d\n" i)))
    (let ((lines (mapcar (lambda (i) (format "missing-%04d" i))
                         (number-sequence 0 1500))))
      (should-not (mevedel-view--live-tail-lines-rendered-position
                   lines (point-max))))))

(mevedel-deftest mevedel-view--full-rerender ()
  ,test
  (test)
  :doc "rebuilds view from data buffer"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** What is 2+2?\n" nil)
    (mevedel-view-test--insert-data data-buf "The answer is 4.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text1 (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (string-match-p "What is 2\\+2" text1))
        (mevedel-view--full-rerender)
        (let ((text2 (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should (string-match-p "What is 2\\+2" text2))
          (should (string-match-p "answer is 4" text2))))))
  :doc "records elapsed timing when render debug is enabled"
  (let ((mevedel-view-render-debug t)
        (mevedel-view-render-debug-buffer-name
         " *mevedel-view-full-rerender-test*"))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
          (mevedel-view-test--insert-data data-buf "Response\n" 'response)
          (with-current-buffer view-buf
            (mevedel-view--full-rerender))
          (with-current-buffer (get-buffer mevedel-view-render-debug-buffer-name)
            (goto-char (point-min))
            (should (search-forward "full-rerender-after-render" nil t))
            (should (search-forward ":elapsed" nil t))))
      (when-let* ((buf (get-buffer mevedel-view-render-debug-buffer-name)))
        (kill-buffer buf))))
  :doc "suppresses modification hooks while rebuilding rendered transcript"
  (mevedel-view-test--with-buffers
    (let ((changes 0))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Response\n" 'response)
      (with-current-buffer view-buf
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--full-rerender)
        (should (= 0 changes))
        (should (string-match-p
                 "Response"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))))
  :doc "suppresses hooks while cleaning stale pending lines"
  (mevedel-view-test--with-buffers
    (let ((changes 0))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Response\n" 'response)
      (with-current-buffer view-buf
        (let ((mevedel-view--pending-tool-calls
               (list (cons 'read "Calling Read…"))))
          (mevedel-view--insert-pending-tool-lines
           mevedel-view--pending-tool-calls))
        (setq mevedel-view--pending-tool-calls nil)
        (add-hook 'after-change-functions
                  (lambda (&rest _ignore)
                    (cl-incf changes))
                  nil t)
        (mevedel-view--full-rerender)
        (should (= 0 changes))
        (should-not (text-property-any
                     (point-min) mevedel-view--input-marker
                     'mevedel-view-pending-tool-live t)))))
  :doc "restores task status fragment after full rerender"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project
                :id "/tmp/view-task/"
                :root "/tmp/view-task/"
                :name "view-task"))
           (session (mevedel-session-create "main" ws)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf)
        (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
        (mevedel-view-test--insert-data data-buf "Response\n" 'response))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-tool-task--display-overlay)
        (should (string-match-p "visible task" (buffer-string)))
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "visible task" text))
          (goto-char (point-min))
          (search-forward "visible task" mevedel-view--input-marker)
          (should (eq 'status (get-text-property
                               (1- (point))
                               'mevedel-view-fragment-namespace)))
          (should (eq 'tasks (get-text-property
                              (1- (point))
                              'mevedel-view-fragment-id)))
          (should-not (mevedel-session-task-overlay session))))))
  :doc "rebuilds status and permission zones in order after full rerender"
  (let ((mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let* ((ws (mevedel-workspace--create
                  :type 'project
                  :id "/tmp/view-zones/"
                  :root "/tmp/view-zones/"
                  :name "view-zones"))
             (session (mevedel-session-create "main" ws))
             (permission-outcomes nil))
        (setf (mevedel-session-tasks session)
              (list (mevedel-task--create
                     :id 1 :subject "visible zone task"
                     :status 'pending)))
        (setf (mevedel-session-permission-queue session)
              (list (list :kind 'generic
                          :tool-name "Read"
                          :specifier-key :path
                          :specifier-value "/tmp/zones.txt"
                          :include-always t
                          :session session
                          :callback
                          (lambda (outcome)
                            (push outcome permission-outcomes)))))
        (with-current-buffer data-buf
          (setq-local mevedel--session session)
          (setq-local mevedel--view-buffer view-buf)
          (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
          (mevedel-view-test--insert-data data-buf "Response\n" 'response))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          ;; Render task status before the full rerender so the fragment
          ;; region is rebuilt along with the other chrome zones.
          (mevedel-tool-task--display-overlay)
          (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                     (lambda ()
                       (list (list :agent-id "verifier--zones123"
                                   :status 'running
                                   :agent-type "verifier"
                                   :description "verify zones"
                                   :calls 1)))))
            (mevedel-view--full-rerender))
          (should-not permission-outcomes)
          (let* ((text (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))
                 (header (string-trim-right
                          (mevedel-view--header-string data-buf)))
                 (header-pos (string-search header text))
                 (task-pos (string-search "visible zone task" text))
                 (agent-pos (string-search
                             "Agent: verifier -- verify zones" text))
                 (permission-pos (string-search "Permission Request"
                                                text)))
            (should header-pos)
            (should task-pos)
            (should agent-pos)
            (should permission-pos)
            (should (= 0 header-pos))
            (should (< header-pos task-pos))
            (should (< task-pos agent-pos))
            (should (< agent-pos permission-pos))
            (should (= 1 (how-many "Permission Request"
                                   (point-min)
                                   mevedel-view--input-marker))))))))
  :doc "does not restore task block after full rerender when all tasks are completed"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project
                :id "/tmp/view-task-completed/"
                :root "/tmp/view-task-completed/"
                :name "view-task-completed"))
           (session (mevedel-session-create "main" ws)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "completed task" :status 'completed)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf)
        (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
        (mevedel-view-test--insert-data data-buf "Response\n" 'response))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "tasks" text))
          (should-not (string-match-p "completed task" text))
          (should-not (mevedel-session-task-overlay session))))))
  :doc "header stays at top when rerendering (input-marker advances past it)"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Greetings\n" nil)
    (mevedel-view-test--insert-data data-buf "Hello back\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (header (mevedel-view--header-string data-buf))
             (header-trim (string-trim-right header))
             (header-pos (string-search header-trim text))
             (greet-pos (string-search "Greetings" text)))
        (should header-pos)
        (should greet-pos)
        (should (< header-pos greet-pos)))))
  :doc "normalizes stale reasoning response prefixes during full rerender"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (reasoning-start nil-start response-start
            response-end)
        (setq reasoning-start (point))
        (insert "#+begin_reasoning\nThinking.\n#+end_reasoning\n")
        (insert "\n")
        (insert "Whi")
        (setq nil-start (point))
        (insert "l")
        (setq response-start (point))
        (insert "e the agents run, I'll test.\n")
        (setq response-end (point))
        (put-text-property reasoning-start nil-start 'gptel 'ignore)
        (put-text-property response-start response-end 'gptel 'response)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "While the agents run, I'll test" text))
        (should-not (string-match-p "^le the agents run" text)))))
  :doc "normalizes restored props in read-only data buffers"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (prefix-start)
        (let ((inhibit-read-only t)
              reasoning-start nil-start response-start response-end)
          (setq reasoning-start (point))
          (insert "#+begin_reasoning\nThinking.\n#+end_reasoning\n\n")
          (setq prefix-start (point))
          (insert "Whi")
          (setq nil-start (point))
          (insert "l")
          (setq response-start (point))
          (insert "e the agents run, I'll test.\n")
          (setq response-end (point))
          (put-text-property reasoning-start nil-start 'gptel 'ignore)
          (put-text-property response-start response-end 'gptel 'response)
          (setq buffer-read-only t))
        (mevedel-view--restore-gptel-bounds-if-needed)
        (should buffer-read-only)
        (should (eq (get-text-property prefix-start 'gptel) 'response)))))
  :doc "renders queued message batches as user follow-ups"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-view--queued-user-message-batch-block
      (list (list :model-input "You can leave out the untracked files")))
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Queued message" text))
        (should (string-match-p "You can leave out the untracked files" text))
        (should-not (string-match-p "<system-reminder>" text))
        (should-not (string-match-p "queued-user-message" text))
        (should-not (string-match-p "Thinking" text)))))
  :doc "renders queued message batches after tool-boundary glue"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_tool (Read :file_path \"/tmp/example\")\n"
     nil)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/example\"))\n\nresult\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data
     data-buf
     (concat "#+end_tool\n\n"
             (mevedel-view--queued-user-message-batch-block
              (list (list :model-input
                          "It is a multiline input with a leading `>` in the composer"))))
     nil)
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_reasoning\nThinking.\n#+end_reasoning\n"
     'ignore)
    (mevedel-view-test--insert-data data-buf "Continuing the answer.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Queued message" text))
        (should (string-match-p
                 "It is a multiline input with a leading `>` in the composer"
                 text))
        (should (string-match-p "Read.*example" text))
        (should (string-match-p "Continuing the answer" text))
        (should-not (string-match-p "#\\+begin_tool" text))
        (should-not (string-match-p "<system-reminder>" text))
        (should-not (string-match-p "queued-user-message" text)))))
  :doc "literal queued XML in user prose is not treated as control markup"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "*** Here is a rendering bug example:\n\n<system-reminder>\nnot generated\n</system-reminder>\n\n<queued-user-message-batch count=\"1\">\n<queued-user-message index=\"1\">\nKeep this literal.\n</queued-user-message>\n</queued-user-message-batch>\n\nThis should render as typed.\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Here is a rendering bug example" text))
        (should (string-match-p "<system-reminder>" text))
        (should (string-match-p "<queued-user-message-batch count=\"1\">" text))
        (should (string-match-p "Keep this literal" text))
        (should-not (string-match-p "Queued message" text)))))
  :doc "renders generated system reminders as compact control rows"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<system-reminder>\nCRITICAL: verify only.\nReport findings.\n</system-reminder>\n"
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "System reminder (2 lines)" text))
        (should-not (string-match-p "Thinking" text))
        (should-not (string-match-p "<system-reminder>" text)))
      (goto-char (point-min))
      (search-forward "System reminder")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type)
                  'system-reminder-summary))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "CRITICAL: verify only" text))
        (should (string-match-p "Report findings" text))
        (should-not (string-match-p "<system-reminder>" text))
        (should-not (string-match-p "</system-reminder>" text)))))
  :doc "keeps generated system reminders separate from real thinking"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Answer first.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "<system-reminder>\nUse verification mode.\n</system-reminder>\n"
     'ignore)
    (mevedel-view-test--insert-data data-buf "\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_reasoning\nInspect the diff.\n#+end_reasoning\n"
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Answer first" text))
        (should (string-match-p "System reminder (1 line)" text))
        (should (string-match-p "Thinking... (1 lines)" text))
        (should-not (string-match-p "<system-reminder>" text)))))
  :doc "omits nested tool blocks from rendered reasoning text"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "#+begin_reasoning\n"
             "I will inspect the file.\n"
             "#+begin_tool (Read :file_path \"a.el\")\n"
             "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
             "tool result line\n"
             "#+end_tool\n"
             "Then I will summarize.\n"
             "#+end_reasoning\n")
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 2 (how-many "Thinking\\.\\.\\. (1 lines)"
                                (point-min)
                                mevedel-view--input-marker)))
        (should (string-match-p "Read: a.el" text))
        (should-not (string-match-p "tool result line" text))
        (should-not (string-match-p "(:name \\\"Read\\\"" text)))
      (goto-char (point-min))
      (search-forward "Thinking...")
      (mevedel-view-toggle-section)
      (goto-char (point-min))
      (search-forward "Thinking...")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "I will inspect the file" text))
        (should (string-match-p "Then I will summarize" text))
        (should-not (string-match-p "tool result line" text))
        (should-not (string-match-p "(:name \\\"Read\\\"" text)))))
  :doc "render-data-only segments after responses stay hidden"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Handoff\n\nBody.\n" nil)
    (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind inline-skill :name "handoff" :arguments ""
              :display-text "/handoff"))
     'ignore)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Handoff" text))
        (should (string-match-p "Assistant answer" text))
        (should-not (string-match-p "mevedel-render-data" text))
        (should-not (string-match-p "inline-skill" text))
        (should-not (string-match-p "Thinking" text)))))
  :doc "separates response prose from following activity"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"a.el\"))\n\ncontents\n"
     '(tool . "call_visual"))
    (mevedel-view-test--insert-data data-buf "Second answer.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let (first-end tool-pos second-pos rule-pos)
        (goto-char (point-min))
        (search-forward "First answer")
        (setq first-end (point))
        (search-forward "Read")
        (setq tool-pos (match-beginning 0))
        (search-forward "Second answer")
        (setq second-pos (match-beginning 0))
        (let ((pos first-end))
          (while (and (< pos tool-pos) (not rule-pos))
            (when (eq (get-text-property pos 'font-lock-face)
                      'mevedel-view-activity-rule)
              (setq rule-pos pos))
            (setq pos (1+ pos))))
        (should rule-pos)
        (should (string-match-p "\n\n"
                                (buffer-substring-no-properties
                                 tool-pos second-pos))))))
  :doc "skips leading :PROPERTIES: drawer on data buffer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      ;; Simulate gptel-org's state drawer at buffer start.
      (insert ":PROPERTIES:\n"
              ":GPTEL_MODEL: test\n"
              ":GPTEL_BOUNDS: ((response (100 200)))\n"
              ":END:\n\n"))
    (mevedel-view-test--insert-data data-buf "*** Actual prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p ":GPTEL_MODEL:" text))
        (should-not (string-match-p ":PROPERTIES:" text))
        (should     (string-match-p "Actual prompt" text))
        (should     (string-match-p "Actual reply" text)))))

  :doc "restores saved GPTEL_BOUNDS before rendering a persisted segment"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n"
              ":GPTEL_MODEL: test\n"
              ":GPTEL_BOUNDS: nil\n"
              ":END:\n\n"
              "*** Prompt\n\n"
              "Assistant intro.\n"
              "\n#+begin_tool (Read :file_path \"/tmp/a.png\")\n"
              "(:name \"Read\" :args (:file_path \"/tmp/a.png\"))\n\n"
              "<media-file>\n"
              "data:\n"
              "<native media block attached>\n"
              "</media-file>\n"
              "#+end_tool\n"
              "Assistant close.\n")
      ;; `org-entry-put' changes the drawer length, so recompute from
      ;; content anchors after each update until the stored positions are
      ;; aligned with the final buffer text.
      (dotimes (_ 3)
        (let (response-start response-end tool-start tool-end close-start
                             close-end)
          (goto-char (point-min))
          (search-forward "Assistant intro.")
          (setq response-start (match-beginning 0)
                response-end (line-end-position))
          (search-forward "#+begin_tool")
          (setq tool-start (match-beginning 0))
          (search-forward "#+end_tool")
          (setq tool-end (line-end-position))
          (search-forward "Assistant close.")
          (setq close-start (match-beginning 0)
                close-end (line-end-position))
          (org-entry-put
           (point-min) "GPTEL_BOUNDS"
           (prin1-to-string
            `((tool (,tool-start ,tool-end "call_1"))
              (response (,response-start ,response-end)
                        (,close-start ,close-end))))))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Prompt" text))
        (should (string-match-p "Assistant intro" text))
        (should (string-match-p "Read: \\(?:/tmp/\\)?a\\.png" text))
        (should (string-match-p "Assistant close" text))
        (should-not (string-match-p "GPTEL_BOUNDS" text))
        (should-not (string-match-p ":PROPERTIES:" text))
        (should-not (string-match-p "<media-file>" text))
        (should-not (string-match-p "(:name \"Read\"" text)))))

  :doc "preserves tool and thinking fold state across full rerender"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"input.pdf\"))\n\nfirst\n"
     '(tool . "call_0"))
    (mevedel-view-test--insert-data data-buf "thinking\nmore thinking\n" 'ignore)
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"output.pdf\"))\n\nline\n"
     '(tool . "call_1"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Thinking...")
        (mevedel-view-toggle-section)
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (mevedel-view-toggle-section)
        (should-not (get-text-property (point) 'mevedel-view-collapsed)))
      (mevedel-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "thinking")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (mevedel-view-toggle-section)
        (should (get-text-property (point) 'mevedel-view-collapsed)))
      (mevedel-view--full-rerender)
      (save-excursion
        (goto-char (point-min))
        (search-forward "thinking")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (goto-char (point-min))
        (search-forward "input.pdf")
        (should (get-text-property (point) 'mevedel-view-collapsed)))))

  :doc "preserves source-backed agent handle state across full rerender"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "FullStateAgent"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "Agent: verifier -- full state"
                        :body result
                        :body-mode 'text-mode
                        :vtype 'agent-handle
                        :initially-collapsed-p t))))
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"FullStateAgent\" :args (:subagent_type \"verifier\"))\n\nfull rerender agent body\n"
     '(tool . "call_full_state_agent"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (should (search-forward "full rerender agent body"
                              mevedel-view--input-marker t))
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "Agent: verifier -- full state")
      (goto-char (match-beginning 0))
      (should-not (get-text-property (point) 'mevedel-view-collapsed))
      (should (search-forward "full rerender agent body"
                              mevedel-view--input-marker t))))

  :doc "does not carry fold state to rewritten data at the same source start"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "RewriteStateTool"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (list :header "RewriteStateTool: item"
                        :body result
                        :body-mode 'text-mode
                        :initially-collapsed-p t))))
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"RewriteStateTool\" :args (:id \"old\"))\n\nold expanded body\n"
     '(tool . "call_rewrite_old"))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "RewriteStateTool: item")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (should (search-forward "old expanded body"
                              mevedel-view--input-marker t)))
    (with-current-buffer data-buf
      (erase-buffer)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"RewriteStateTool\" :args (:id \"new\"))\n\nnew body must start collapsed\n"
       '(tool . "call_rewrite_new")))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (goto-char (point-min))
      (search-forward "RewriteStateTool: item")
      (goto-char (match-beginning 0))
      (should (get-text-property (point) 'mevedel-view-collapsed))
      (should-not (search-forward "new body must start collapsed"
                                  mevedel-view--input-marker t))
      (should (= 0 (hash-table-count mevedel-view--source-collapse-states)))))

  :doc "does not carry non-tool fold state to same-prefix rewritten data"
  (mevedel-view-test--with-buffers
    (let ((prefix (make-string 300 ?x)))
      (mevedel-view-test--insert-data
       data-buf (concat prefix "\nold thinking tail\n") 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "old thinking tail"
                                mevedel-view--input-marker t)))
      (with-current-buffer data-buf
        (erase-buffer)
        (mevedel-view-test--insert-data
         data-buf (concat prefix "\nnew thinking starts collapsed\n")
         'ignore))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (should (get-text-property (point) 'mevedel-view-collapsed))
        (should-not (search-forward "new thinking starts collapsed"
                                    mevedel-view--input-marker t)))))

  :doc "reanchors in-flight assistant after restoring earlier expanded fold"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Read\" :args (:file_path \"input.pdf\"))\n\nfirst\nsecond\nthird\n"
       '(tool . "call_0"))
      (mevedel-view-test--insert-data data-buf "\n\n*** Second\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (save-excursion
          (goto-char (point-min))
          (search-forward "input.pdf")
          (mevedel-view-toggle-section)
          (should-not (get-text-property (point) 'mevedel-view-collapsed)))
        (setq mevedel-view--data-turn-start data-turn-start)
        ;; Exercise the legacy integer shape too: the rerender must
        ;; normalize it to a marker that tracks fold-state restoration.
        (setq mevedel-view--in-flight-turn-start
              (marker-position mevedel-view--input-marker))
        (mevedel-view--full-rerender)
        (should (markerp mevedel-view--in-flight-turn-start))
        (save-excursion
          (goto-char mevedel-view--in-flight-turn-start)
          (should (looking-at-p "Assistant"))
          (should (search-forward "Second response"
                                  mevedel-view--input-marker t)))
        (mevedel-view-test--insert-data data-buf "More text.\n" 'response)
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (= 1 (cl-count-if (lambda (line) (string= line "Assistant"))
                                    (split-string text "\n"))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Second response" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "turn fold normalizes legacy in-flight assistant anchor"
  (mevedel-view-test--with-buffers
    (let (data-turn-start second-start)
      (mevedel-view-test--insert-data data-buf "*** First\n" nil)
      (mevedel-view-test--insert-data
       data-buf
       "First response line 1.\nFirst response line 2.\nFirst response line 3.\n"
       'response)
      (mevedel-view-test--insert-data data-buf "\n\n*** Second\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (search-forward "Assistant")
          (setq second-start (match-beginning 0)))
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start second-start)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (mevedel-view-toggle-section))
        (should (markerp mevedel-view--in-flight-turn-start))
        (mevedel-view-test--insert-data data-buf "More text.\n" 'response)
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (= 1 (cl-count-if (lambda (line) (string= line "Assistant"))
                                    (split-string text "\n"))))
          (should (= 1 (cl-loop with start = 0
                                while (string-match "Second response" text start)
                                count t
                                do (setq start (match-end 0)))))))))

  :doc "marks skipped leading compaction summary in rotated segment"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "#+begin_summary mevedel-role=compaction-summary\n")
        (put-text-property start (point) 'gptel 'ignore))
      (insert "Summary should stay out of view.\n")
      (let ((start (point)))
        (insert "#+end_summary\n\n")
        (put-text-property start (point) 'gptel 'ignore)))
    (mevedel-view-test--insert-data data-buf "*** Actual prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Actual reply\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p "Summary should stay out of view" text))
        (should (= 1 (cl-loop with start = 0
                              while (string-match
                                     "conversation compacted"
                                     text start)
                              count t
                              do (setq start (match-end 0)))))
        (should (string-match-p "Actual prompt" text))
        (should (string-match-p "Actual reply" text)))))

  :doc "preserves in-flight live tail when data has no assistant replacement yet"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Read files\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\n... Thinking... (1 lines)\nCalling Read...\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Read files" text))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Calling Read" text))))))

  :doc "does not append preserved live tail already rendered from data"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\n*** Follow-up\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\nAssistant answer.\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (mevedel-view--full-rerender)
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n")))
             (answer-count
              (cl-loop with start = 0
                       while (string-match "Assistant answer" text start)
                       count t
                       do (setq start (match-end 0)))))
        (should (string-match-p "Follow-up" text))
        (should (= 1 assistant-count))
        (should (= 1 answer-count))
        (save-excursion
          (goto-char mevedel-view--in-flight-turn-start)
          (should (looking-at-p "Assistant"))))))

  :doc "matches preserved live tail by stable prefix when status lines differ"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "Assistant answer.\n(:name \"Read\" :args (:file_path \"a.el\"))\n\nok\n"
     'response)
    (mevedel-view-test--insert-data data-buf "\n\n*** Follow-up\n" nil)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\nAssistant answer.\n✓ Agent: reviewer done · 9.9s · 3 calls\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n")))
             (answer-count
              (cl-loop with start = 0
                       while (string-match "Assistant answer" text start)
                       count t
                       do (setq start (match-end 0)))))
        (should (string-match-p "Follow-up" text))
        (should (= 1 assistant-count))
        (should (= 1 answer-count))
        (should-not (string-match-p "reviewer done" text)))))

  :doc "task status rerender does not duplicate preserved live tail"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "view-task-rerender"
                       :root temporary-file-directory
                       :name "view-task-rerender"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "Inspect renderer" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (mevedel-view-test--insert-data data-buf "\n\n*** Follow-up\n" nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq start (point))
          (insert "Assistant\nAssistant answer.\n")
          (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (mevedel-view--full-rerender)
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n")))
               (task-count
                (cl-loop with start = 0
                         while (string-match "Inspect renderer" text start)
                         count t
                         do (setq start (match-end 0)))))
          (should (= 1 assistant-count))
          (should (= 1 task-count))))))

  :doc "full rerender recreates progress from request state"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max) nil)))
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--full-rerender)
      (should (mevedel-view--request-progress-visible-p))
      (mevedel-view--stop-spinner)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Prompt" text))
        (should-not (string-match-p "Thinking" text)))))

  :doc "reanchors to current assistant when mailbox follows the in-flight turn"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        ;; Match the send-path marker shape: inside the user run, just
        ;; before the assistant response starts.
        (setq data-turn-start (copy-marker (1- (point)) nil)))
      (mevedel-view-test--insert-data data-buf "Assistant answer.\n" 'response)
      (mevedel-view-test--insert-data
       data-buf
       "\n<agent-message from=\"explorer\">\nhello\n</agent-message>\n"
       nil)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq start (point))
          (insert "Assistant\nstale live tail\n")
          (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start data-turn-start)
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-count
                (cl-count-if (lambda (line) (string= line "Assistant"))
                             (split-string text "\n"))))
          (should (string-match-p "Assistant answer" text))
          (should (string-match-p "hello" text))
          (should-not (string-match-p "stale live tail" text))
          (should (= 1 assistant-count))
          (save-excursion
            (goto-char mevedel-view--in-flight-turn-start)
            (should (looking-at-p "Assistant")))))))
(ert-deftest mevedel-view--full-rerender-in-flight-user-anchor/test ()
  "Full rerender during a new request keeps the in-flight anchor after `You'."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** First\n" nil)
    (mevedel-view-test--insert-data data-buf "First response.\n" 'response)
    (mevedel-view-test--insert-data data-buf "\n\n*** Second\n" nil)
    (with-current-buffer data-buf
      (let ((data-turn-start (copy-marker (point-max) nil)))
        (with-current-buffer view-buf
          (setq mevedel-view--data-turn-start data-turn-start)
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker nil)))))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (should (string-match-p
               "Second"
               (buffer-substring-no-properties
                (point-min) mevedel-view--input-marker))))
    (mevedel-view-test--insert-data data-buf "Second response.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--render-incremental data-buf)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "First response" text))
        (should (string-match-p "Second" text))
        (should (string-match-p "Second response" text))))))


;;
;;; Interaction zone

(mevedel-deftest mevedel-view--interaction-zone-render
  (:doc "renders and rebuilds interaction-zone fragments")
  ,test
  (test)

  :doc "permission and plan descriptors materialize as real text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((map (make-sparse-keymap)))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 2
               :body "\npermission\n" :keymap map
               :help-echo "Permission" :entry 'permission-entry
               :activate #'ignore))
        (mevedel-view--interaction-register
         (list :kind 'plan :id 'plan :count 1
               :body "\nplan\n" :keymap map
               :help-echo "Plan" :entry 'plan-entry
               :activate #'ignore)))
      (should (equal "1 plan · 2 permissions pending"
                     (mevedel-view--interaction-count-label)))
      (goto-char (point-min))
      (search-forward "1 plan · 2 permissions pending"
                      mevedel-view--input-marker)
      (should (eq 'interaction (get-text-property
                                (match-beginning 0)
                                'mevedel-view-fragment-namespace)))
      (should (eq :separator (get-text-property
                              (match-beginning 0)
                              'mevedel-view-fragment-id)))
      (should (overlayp mevedel-view--interaction-region-overlay))
      (should (string-match-p "plan" (buffer-string)))
      (should (string-match-p "permission" (buffer-string)))
      (should (string-match-p "plan\n\n\npermission" (buffer-string)))
      (maphash
       (lambda (_id overlay)
         (should (< (overlay-start overlay) (overlay-end overlay)))
         (should (overlay-get overlay 'mevedel-view-interaction-entry))
         (should (overlay-get overlay 'mevedel-view-interaction-activate))
         (should (overlay-get overlay 'keymap))
         (should-not (overlay-get overlay 'before-string))
         (should (get-text-property
                  (overlay-start overlay)
                  'mevedel-view-interaction-overlay))
         (should-not (get-text-property
                      (overlay-start overlay)
                      'mouse-face)))
       mevedel-view--interaction-overlays)))

  :doc "shared activation does not call interaction descriptor callbacks"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((called nil))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission needs an explicit outcome\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate (lambda () (setq called t))))
        (goto-char (point-min))
        (search-forward "permission needs" mevedel-view--input-marker)
        (should-error (mevedel-view-activate-at-point) :type 'user-error)
        (should-not called))))

  :doc "fragment migration reuses descriptor overlays and metadata"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((map (make-sparse-keymap))
             (overlay
              (mevedel-view--interaction-register
               (list :kind 'preview :id 'preview :count 1
                     :body "\nold body text\n" :keymap map
                     :help-echo "Preview" :entry 'preview-entry
                     :activate #'ignore))))
        (overlay-put overlay 'test-private-state 'kept)
        (goto-char (overlay-start overlay))
        (search-forward "body" (overlay-end overlay))
        (let ((updated
               (mevedel-view--interaction-register
                (list :kind 'preview :id 'preview :count 1
                      :body "\nnew body text\n" :keymap map
                      :help-echo "Preview" :entry 'preview-entry
                      :activate #'ignore))))
          (should (eq overlay updated))
          (should (eq 'kept (overlay-get overlay 'test-private-state)))
          (should (eq overlay
                      (get-text-property
                       (overlay-start overlay)
                       'mevedel-view-interaction-overlay)))
          (should (get-text-property (overlay-start overlay)
                                     'mevedel-view-fragment-key))
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (= 1 (mevedel-view-test--count-substring
                          "new body text" text)))
            (should (= 0 (mevedel-view-test--count-substring
                          "old body text" text))))))))

  :doc "fragment migration preserves legacy spacing for body without trailing newline"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((overlay
             (mevedel-view--interaction-register
              (list :kind 'preview :id 'preview :count 1
                    :body "preview body" :keymap (make-sparse-keymap)
                    :entry 'preview-entry :activate #'ignore))))
        (should (equal "preview body\n"
                       (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay)))))))

  :doc "normalizes raw UTF-8 bytes in interaction descriptor bodies"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((raw (test-mevedel-view--raw-bytes
                  #xe2 #x80 #x9c ?x #xe2 #x80 #x9d)))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body (concat "\npermission " raw "\n")
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore)))
      (should (string-match-p "permission “x”" (buffer-string)))))

  :doc "ignores interaction markers that drift before the status zone"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (set-marker mevedel-view--interaction-marker (point-min))
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "\npermission below status\n"
             :keymap (make-sparse-keymap)
             :entry 'permission-entry
             :activate #'ignore))
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (header (string-trim-right
                      (mevedel-view--header-string data-buf)))
             (header-pos (string-search header text))
             (permission-pos (string-search "permission below status"
                                            text)))
        (should header-pos)
        (should permission-pos)
        (should (< header-pos permission-pos))
        (should (>= (overlay-start
                     mevedel-view--interaction-region-overlay)
                    (marker-position mevedel-view--status-marker))))))

  :doc "ignores jointly drifted status and interaction markers before the header"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project
                :id "/tmp/view-stale-header/"
                :root "/tmp/view-stale-header/"
                :name "view-stale-header"))
           (session (mevedel-session-create "renamed" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (set-marker mevedel-view--status-marker (point-min))
        (set-marker mevedel-view--interaction-marker (point-min))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "\npermission below header\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (header "mevedel")
               (header-pos (string-search header text))
               (permission-pos (string-search "permission below header"
                                              text)))
          (should header-pos)
          (should permission-pos)
          (should (< header-pos permission-pos))
          (should (>= (overlay-start
                       mevedel-view--interaction-region-overlay)
                      (mevedel-view--header-end-position)))))))

  :doc "managed interaction region excludes request-progress spinner"
  (let ((mevedel-view-spinner-animate nil))
    (mevedel-view-test--with-buffers
      (with-current-buffer view-buf
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission\n" :keymap (make-sparse-keymap)
               :entry 'permission-entry :activate #'ignore))
        (mevedel-view--start-spinner "Working...")
        (should (overlayp mevedel-view--interaction-region-overlay))
        (should (overlayp mevedel-view--request-progress-region-overlay))
        (should (<= (overlay-end mevedel-view--interaction-region-overlay)
                    (overlay-start
                     mevedel-view--request-progress-region-overlay)))
        (should-not (string-match-p
                     "Working"
                     (buffer-substring-no-properties
                      (overlay-start mevedel-view--interaction-region-overlay)
                      (overlay-end mevedel-view--interaction-region-overlay)))))))

  :doc "clear-for-rebuild removes rebuild-owned fragment text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'permission-entry :activate #'ignore))
      (mevedel-view--interaction-clear-for-rebuild)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should-not (string-match-p "permission prompt" text))
        (should (= 0 (hash-table-count
                      mevedel-view--interaction-descriptors)))
        (should (= 0 (hash-table-count
                      mevedel-view--interaction-overlays))))))

  :doc "status redraw stays above existing permission interaction"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project :id "/tmp/view-interaction-order/"
                :root "/tmp/view-interaction-order/"
                :name "view-interaction-order"))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "permission prompt\n"
               :keymap (make-sparse-keymap)
               :entry 'permission-entry
               :activate #'ignore))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "verifier--abcdef123456"
                                 :status 'blocked
                                 :agent-type "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19))))
                  ((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'default)))
          (mevedel-view--render-agent-status))
        (let (agent-pos separator-pos prompt-pos)
          (save-excursion
            (goto-char (point-min))
            (search-forward "Agent: verifier -- Verify tracked diff")
            (setq agent-pos (match-beginning 0))
            (search-forward "1 permission pending")
            (setq separator-pos (match-beginning 0))
            (search-forward "permission prompt")
            (setq prompt-pos (match-beginning 0)))
          (should (eq 'status (get-text-property
                               agent-pos
                               'mevedel-view-fragment-namespace)))
          (should (eq 'agents (get-text-property
                               agent-pos 'mevedel-view-fragment-id)))
          (should (eq 'interaction (get-text-property
                                    separator-pos
                                    'mevedel-view-fragment-namespace)))
          (should (eq :separator (get-text-property
                                  separator-pos 'mevedel-view-fragment-id)))
          (should (< agent-pos separator-pos))
          (let ((permission-overlay
                 (gethash 'permission mevedel-view--interaction-overlays)))
            (should (overlayp permission-overlay))
            (should (<= separator-pos (overlay-start permission-overlay))))
          (should (<= separator-pos prompt-pos))))))

  :doc "permission prompt appears below existing agent status"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'project :id "/tmp/view-interaction-after-status/"
                :root "/tmp/view-interaction-after-status/"
                :name "view-interaction-after-status"))
           (session (mevedel-session-create "main" ws))
           (outcomes nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "verifier--abcdef123456"
                                 :status 'blocked
                                 :agent-type "verifier"
                                 :description "Verify tracked diff"
                                 :calls 19)))))
          (mevedel-view--render-agent-status))
        (should (string-match-p
                 "Agent: verifier -- Verify tracked diff"
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))))
      (with-current-buffer data-buf
        (mevedel-permission--enqueue
         (list :kind 'generic
               :tool-name "Read"
               :specifier-key :path
               :specifier-value "/tmp/after-status.txt"
               :include-always nil
               :origin "verifier--abcdef123456"
               :callback (lambda (outcome) (push outcome outcomes)))
         session))
      (with-current-buffer view-buf
        (should-not outcomes)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (agent-pos (string-search
                           "Agent: verifier -- Verify tracked diff" text))
               (permission-pos (string-search "Permission Request" text)))
          (should agent-pos)
          (should permission-pos)
          (should (< agent-pos permission-pos))
          (should (string-search "1 permission pending" text))
          (goto-char (point-min))
          (search-forward "1 permission pending" mevedel-view--input-marker)
          (should (eq :separator (get-text-property
                                  (match-beginning 0)
                                  'mevedel-view-fragment-id)))))))

  :doc "permission queue renders only the FIFO head while request progress is visible"
  (let ((mevedel-view-spinner-animate nil)
        (mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let ((session (mevedel-session--create
                      :name "test"
                      :workspace nil
                      :permission-rules nil
                      :permission-mode 'default
                      :permission-queue nil
                      :plan-queue nil))
            outcomes)
        (with-current-buffer data-buf
          (setq-local mevedel--session session)
          (setq-local mevedel--view-buffer view-buf))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (mevedel-view--start-spinner "Working..."))
        (cl-letf (((symbol-function 'gptel-agent--block-bg)
                   (lambda () 'default)))
          (with-current-buffer data-buf
            (dolist (path '("/tmp/one.el" "/tmp/two.el" "/tmp/three.el"))
              (let ((captured-path path))
                (mevedel-permission--enqueue
                 (list :kind 'generic
                       :tool-name "Read"
                       :specifier-key :path
                       :specifier-value captured-path
                       :include-always nil
                       :origin "main"
                       :callback
                       (lambda (outcome)
                         (push (cons captured-path outcome) outcomes)))
                 session)))))
        (with-current-buffer view-buf
          (cl-labels
              ((display-text ()
                 (buffer-substring-no-properties
                  (point-min) mevedel-view--input-marker))
               (head-overlay ()
                 (let* ((entry (car (mevedel-session-permission-queue
                                     session)))
                        (id (mevedel-queue--entry-metadata-get
                             entry :interaction-id)))
                   (and id (gethash id mevedel-view--interaction-overlays))))
               (settle-head ()
                 (mevedel--prompt--settle (head-overlay) 'allow-once))
               (should-show (count visible-path hidden-paths)
                 (let ((text (display-text)))
                   (should (= 1 (mevedel-view-test--count-substring
                                 "Permission Request" text)))
                   (should (string-search
                            (format "%d permission%s pending"
                                    count (if (= count 1) "" "s"))
                            text))
                   (should (string-search visible-path text))
                   (dolist (path hidden-paths)
                     (should-not (string-search path text))))))
            (should-show 3 "/tmp/one.el" '("/tmp/two.el" "/tmp/three.el"))
            (settle-head)
            (should-show 2 "/tmp/two.el" '("/tmp/one.el" "/tmp/three.el"))
            (settle-head)
            (should-show 1 "/tmp/three.el" '("/tmp/one.el" "/tmp/two.el"))
            (settle-head)
            (let ((text (display-text)))
              (should (= 0 (mevedel-view-test--count-substring
                            "Permission Request" text)))
              (should-not (string-search "permission pending" text))
              (should-not (string-search "permissions pending" text))
              (should-not (string-search "/tmp/one.el" text))
              (should-not (string-search "/tmp/two.el" text))
              (should-not (string-search "/tmp/three.el" text)))
            (should (equal '(("/tmp/three.el" . allow-once)
                             ("/tmp/two.el" . allow-once)
                             ("/tmp/one.el" . allow-once))
                           outcomes)))))))

  :doc "render removes interaction fragment text from a dead region overlay"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'old-permission :count 1
             :body "old permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'old-entry :activate #'ignore))
      (delete-overlay mevedel-view--interaction-region-overlay)
      (setq mevedel-view--interaction-region-overlay nil)
      (mevedel-view--interaction-clear-for-rebuild)
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'new-permission :count 1
             :body "new permission prompt\n" :keymap (make-sparse-keymap)
             :entry 'new-entry :activate #'ignore))
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 0 (mevedel-view-test--count-substring
                      "old permission prompt" text)))
        (should (= 1 (mevedel-view-test--count-substring
                      "new permission prompt" text))))))

  :doc "clears stale interaction prompt overlays without settling"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((outcomes nil)
            (ov (make-overlay (point-min) (point-min)
                              (current-buffer) nil t)))
        (overlay-put ov 'mevedel-view-interaction-id 'stale-permission)
        (overlay-put ov 'mevedel-user-request t)
        (overlay-put ov 'mevedel--callback
                     (lambda (outcome) (push outcome outcomes)))
        (setq mevedel--prompt-overlays (list ov))
        (mevedel-view--interaction-clear)
        (should-not (overlay-buffer ov))
        (should-not mevedel--prompt-overlays)
        (should-not outcomes))))

  :doc "rebuild preserves direct request and ask prompts without settling"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let (overlays
            outcomes)
        (dolist (kind '(request ask))
          (let* ((captured-kind kind)
                 (id (list captured-kind))
                 (ov
                  (mevedel-view--interaction-register
                   (list :kind captured-kind
                         :id id
                         :count 0
                         :body (format "\n%s prompt\n" captured-kind)
                         :keymap (make-sparse-keymap)
                         :activate
                         (lambda (outcome)
                           (push (cons captured-kind outcome) outcomes))))))
            (overlay-put ov 'mevedel-user-request t)
            (overlay-put ov 'mevedel--callback
                         (lambda (outcome)
                           (push (cons captured-kind outcome) outcomes)))
            (push (cons captured-kind ov) overlays)
            (cl-pushnew ov mevedel--prompt-overlays :test #'eq)))
        (mevedel-view--interaction-rebuild)
        (should-not outcomes)
        (should (= 2 (length mevedel--prompt-overlays)))
        (dolist (pair overlays)
          (let* ((kind (car pair))
                 (ov (cdr pair))
                 (id (list kind)))
            (should (eq ov (gethash id
                                    mevedel-view--interaction-overlays)))
            (should (overlay-buffer ov))
            (should (gethash id mevedel-view--interaction-descriptors))))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "request prompt" text))
          (should (string-match-p "ask prompt" text)))
        (should (equal "1 request · 1 question pending"
                       (mevedel-view--interaction-count-label)))
        (mevedel--prompt--settle (cdr (assq 'request overlays)) 'deny)
        (mevedel--prompt--settle (cdr (assq 'ask overlays)) 'aborted)
        (should (member '(request . deny) outcomes))
        (should (member '(ask . aborted) outcomes)))))

  :doc "does not focus interaction prompt while a live window is drafting"
  (mevedel-view-test--with-buffers
    (switch-to-buffer view-buf)
    (delete-other-windows)
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "> quoted\nsecond line")
      (goto-char (+ (mevedel-view--input-start) 4))
      ;; Simulate buffer point drifting away from the selected window point.
      ;; Prompt focus must respect the live cursor, not this stale value.
      (save-excursion
        (goto-char (point-min))))
    (with-current-buffer view-buf
      (let ((overlay
             (mevedel-view--interaction-register
              (list :kind 'permission :id 'permission :count 1
                    :body "\npermission\n" :keymap (make-sparse-keymap)
                    :help-echo "Permission" :entry 'permission-entry
                    :activate #'ignore))))
        (should (= (window-point (selected-window))
                   (+ (mevedel-view--input-start) 4)))
        (should (string= "> quoted\nsecond line" (mevedel-view--input-text)))
        (should (overlay-get overlay 'read-only))
        (should (get-text-property (overlay-start overlay) 'read-only))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only))))
    (delete-other-windows))

  :doc "redraw regression: interaction update/rebuild preserves multiline > composer and removes stale body"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      ;; Status/task redraw coverage for this composer shape lives in
      ;; `mevedel-tool-task--display-overlay'; this case fills the
      ;; interaction-zone redraw gap before the fragment migration.
      (let* ((draft "> first line\nsecond line")
             (point-offset (length "> first"))
             (old-body "old preview body")
             (current-body "current preview body")
             (map (make-sparse-keymap)))
        (cl-labels
            ((display-text ()
               (buffer-substring-no-properties
                (point-min) mevedel-view--input-marker))
             (should-preserve-composer ()
               (should (string= draft (mevedel-view--input-text)))
               (should (= (point)
                          (+ (mevedel-view--input-start) point-offset)))
               (should (< (point) (point-max)))
               (should (equal " line"
                              (buffer-substring-no-properties
                               (point)
                               (min (point-max)
                                    (+ (point) (length " line"))))))
               (should-not (get-text-property (mevedel-view--input-start)
                                              'read-only)))
             (should-show-current-body ()
               (let ((display (display-text)))
                 (should (= 1 (mevedel-view-test--count-substring
                               current-body display)))
                 (should (= 0 (mevedel-view-test--count-substring
                               old-body display)))
                 (should (equal "1 preview pending"
                                (mevedel-view--interaction-count-label)))))
             (should-clear-bodies ()
               (let ((display (display-text)))
                 (should (= 0 (mevedel-view-test--count-substring
                               current-body display)))
                 (should (= 0 (mevedel-view-test--count-substring
                               old-body display))))))
          (mevedel-view-test--insert-composer-draft draft point-offset)
          (mevedel-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (concat "\n" old-body "\n")
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (mevedel-view--interaction-register
           (list :kind 'preview :id 'preview :count 1
                 :body (concat "\n" current-body "\n")
                 :keymap map :help-echo "Preview" :activate #'ignore))
          (should-preserve-composer)
          (should-show-current-body)
          (mevedel-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (mevedel-view--interaction-rebuild)
          (should-preserve-composer)
          (should-show-current-body)
          (mevedel-view--interaction-clear)
          (should-preserve-composer)
          (should-clear-bodies)))))

  :doc "incremental history render stays above fragment-backed interaction UI"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Read files\n" nil)
    (mevedel-view-test--insert-data data-buf "Working through it.\n" 'response)
    (with-current-buffer view-buf
      (let ((map (make-sparse-keymap)))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min) t)))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--status-marker t))
        (mevedel-view--interaction-register
         (list :kind 'permission :id 'permission :count 1
               :body "\npermission\n" :keymap map
               :help-echo "Permission" :entry 'permission-entry
               :activate #'ignore))
        (cl-letf (((symbol-function 'mevedel-view--interaction-rebuild)
                   #'ignore))
          (mevedel-view--render-incremental data-buf))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (assistant-pos (string-match-p "Assistant" text))
               (permission-pos (string-match-p "permission" text)))
          (should assistant-pos)
          (should permission-pos)
          (should (< assistant-pos permission-pos)))
        (mevedel-view--interaction-clear)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Working through it" text))
          (should-not (string-match-p "permission" text))))))

  :doc "rerender rebuilds from live queues without settling"
  (let ((mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'default
                       :permission-queue nil
                       :plan-queue nil))
             (plan-outcomes nil)
             (permission-outcomes nil))
        (with-current-buffer data-buf
          (setq-local mevedel--session session))
        (with-current-buffer view-buf
          (setq-local mevedel--session session)
          (setf (mevedel-session-plan-queue session)
                (list (list :body "# Plan"
                            :chat-buffer data-buf
                            :session session
                            :callback
                            (lambda (outcome)
                              (push outcome plan-outcomes)))))
          (setf (mevedel-session-permission-queue session)
                (list (list :kind 'generic
                            :tool-name "Read"
                            :specifier-value "/tmp/file.txt"
                            :include-always t
                            :session session
                            :callback
                            (lambda (outcome)
                              (push outcome permission-outcomes)))))
          (mevedel-view--interaction-rebuild)
          (should-not plan-outcomes)
          (should-not permission-outcomes)
          (should (equal "1 plan · 1 permission pending"
                         (mevedel-view--interaction-count-label)))
          (let ((kinds nil))
            (maphash
             (lambda (_id descriptor)
               (push (plist-get descriptor :kind) kinds)
               (should (plist-member descriptor :entry))
               (should (plist-get descriptor :activate)))
             mevedel-view--interaction-descriptors)
            (should (memq 'plan kinds))
            (should (memq 'permission kinds))))))))

  :doc "agent-owned permission prompt stays in parent view after parent request end"
  (let ((mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create
                       :name "test"
                       :workspace nil
                       :permission-rules nil
                       :permission-mode 'default
                       :permission-queue nil
                       :plan-queue nil))
             (agent (mevedel-agent--create :name "verifier"))
             (inv (mevedel-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-perm*"))
             (outcomes nil))
        (unwind-protect
            (progn
              (setf (mevedel-agent-invocation-agent-id inv) "verifier--abc")
              (setf (mevedel-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (setf (mevedel-agent-invocation-parent-session inv)
                    session)
              (with-current-buffer data-buf
                (setq-local mevedel--session session)
                (mevedel-request-begin session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel--agent-invocation inv)
                (setq-local mevedel--view-buffer view-buf))
              (cl-letf (((symbol-function 'gptel-agent--block-bg)
                         (lambda () 'default)))
                (with-current-buffer agent-buf
                  (mevedel-permission--enqueue
                   (list :kind 'generic
                         :tool-name "Read"
                         :specifier-key :path
                         :specifier-value "/tmp/from-agent.txt"
                         :include-always nil
                         :origin "verifier--abc"
                         :callback
                         (lambda (outcome)
                           (push outcome outcomes)))
                   session)))
              (let* ((entry (car (mevedel-session-permission-queue session)))
                     (interaction-id
                      (mevedel-queue--entry-metadata-get
                       entry :interaction-id)))
                (should interaction-id)
                (with-current-buffer view-buf
                  (should (gethash interaction-id
                                   mevedel-view--interaction-overlays))
                  (should (string-match-p
                           "Permission Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max))))
                  (should-not mevedel--prompt-overlays))
                (with-current-buffer agent-buf
                  (should-not
                   (string-match-p
                    "Permission Request"
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
                (with-current-buffer data-buf
                  (mevedel-request-end))
                (should-not outcomes)
                (should (= 1 (length
                              (mevedel-session-permission-queue session))))
                (with-current-buffer view-buf
                  (should (gethash interaction-id
                                   mevedel-view--interaction-overlays))
                  (cl-letf (((symbol-function 'gptel-agent--block-bg)
                             (lambda () 'default)))
                    (mevedel-view--full-rerender))
                  (should-not outcomes)
                  (should (= 1 (length
                                (mevedel-session-permission-queue
                                 session))))
                  (should (string-match-p
                           "Permission Request"
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))))
          (when (buffer-live-p agent-buf)
            (kill-buffer agent-buf))))))

  :doc "interaction target ignores transcript inspection view"
  (let ((mevedel-session-persistence nil))
    (mevedel-view-test--with-buffers
      (let* ((session (mevedel-session--create :name "test"))
             (agent (mevedel-agent--create :name "verifier"))
             (inv (mevedel-agent-invocation-create agent))
             (agent-buf (generate-new-buffer " *test-agent-data*"))
             (agent-view (generate-new-buffer " *test-agent-view*")))
        (unwind-protect
            (progn
              (setf (mevedel-agent-invocation-agent-id inv) "verifier--abc")
              (setf (mevedel-agent-invocation-parent-data-buffer inv)
                    data-buf)
              (with-current-buffer data-buf
                (setq-local mevedel--session session))
              (with-current-buffer view-buf
                (setq-local mevedel--session session))
              (with-current-buffer agent-buf
                (org-mode)
                (setq-local mevedel--session session)
                (setq-local mevedel--agent-invocation inv)
                (setq-local mevedel--view-buffer view-buf))
              (mevedel-view--setup
               agent-view agent-buf
               (list :agent-transcript-p t
                     :agent-id "verifier--abc"
                     :parent-view view-buf
                     :preserve-data-view-buffer t
                     :transcript-info
                     (list :agent-id "verifier--abc"
                           :status 'running
                           :buffer agent-buf
                           :live-buffer t
                           :session session)))
              (with-current-buffer agent-view
                (should (eq view-buf
                            (mevedel-view--interaction-target-buffer
                             agent-buf)))))
          (when (buffer-live-p agent-view) (kill-buffer agent-view))
          (when (buffer-live-p agent-buf) (kill-buffer agent-buf))))))

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


;;
;;; Expand/collapse

(mevedel-deftest mevedel-view-toggle-section ()
  ,test
  (test)
  :doc "expands collapsed tool one-liners"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f.el\"))\n\nfull content here\nline 2\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "Done.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      ;; Find the collapsed tool line
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (< (point) mevedel-view--input-marker))
          (if (eq (get-text-property (point) 'mevedel-view-collapsed) t)
              (setq found t)
            (goto-char (or (next-single-property-change
                            (point) 'mevedel-view-collapsed)
                           mevedel-view--input-marker))))
        (should found)
        ;; Content should be collapsed
        (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "full content here" text)))
        ;; Expand
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
          (should (string-match-p "full content here" text))))))

  :doc "non-expandable tool events remain non-toggleable and untracked"
  (mevedel-view-test--with-buffers
    (mevedel-tool-register
     (mevedel-tool--create
      :name "EventStateTool"
      :category "mevedel"
      :renderer (lambda (_name _args _result _data)
                  (list :header "EventStateTool: complete"
                        :expandable-p nil))))
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"EventStateTool\" :args (:id 1))\n\nevent body hidden\n"
     '(tool . "call_event_state"))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char mevedel-view--input-marker)
        (mevedel-view--render-tool-group
         (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
         data-buf))
      (goto-char (point-min))
      (search-forward "EventStateTool: complete")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type)
                  'tool-event))
      (should-not (get-text-property (point) 'mevedel-view-source))
      (should-error (mevedel-view-toggle-section) :type 'user-error)
      (should (= 0 (hash-table-count mevedel-view--source-collapse-states)))
      (should-not (search-forward "event body hidden"
                                  mevedel-view--input-marker t)))))

(mevedel-deftest mevedel-view-toggle-section/renderer-vtype ()
  ,test
  (test)
  :doc "prompt summaries expand and collapse through their renderer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
    (with-current-buffer view-buf
      (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
             (rendering '(:header "Agent: explorer -- Find calls"
                          :body "rendered agent body\n"
                          :body-mode text-mode
                          :vtype prompt-summary)))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert "› Agent: explorer -- Find calls\nrendered agent body\n")
                (add-text-properties
                 start (point)
                 `(font-lock-face mevedel-view-tool-summary
                   mevedel-view-type prompt-summary
                   mevedel-view-collapsed nil
                   mevedel-view-source ,source
                   read-only t
                   keymap ,mevedel-view--display-map
                   front-sticky (read-only keymap)
                   rear-nonsticky (read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                   (lambda (buf start end &optional _collapsed-only)
                     (should (eq buf data-buf))
                     (should (= start (car source)))
                     (should (= end (cdr source)))
                     rendering)))
          (goto-char (point-min))
          (search-forward "rendered agent body")
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "Agent: explorer -- Find calls" text))
            (should-not (string-match-p "rendered agent body" text))
            (should-not (string-match-p "raw launch payload" text))
            (goto-char (point-min))
            (search-forward "Agent: explorer")
            (goto-char (match-beginning 0))
            (should (eq (get-text-property (point) 'mevedel-view-type)
                        'prompt-summary))
            (should (eq (get-text-property (point)
                                           'mevedel-view-collapsed)
                        t)))
          (goto-char (point-min))
          (search-forward "Agent: explorer")
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rendered agent body" text))
            (should-not (string-match-p "raw launch payload" text))
            (goto-char (point-min))
            (search-forward "Agent: explorer")
            (goto-char (match-beginning 0))
            (should (eq (get-text-property (point)
                                           'mevedel-view-collapsed)
                        nil))))))))

  :doc "agent handles without transcript ids still expand inline"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
    (with-current-buffer view-buf
      (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
             (rendering (mevedel-tool-ui--render-agent
                         "Agent"
                         '(:subagent_type "explorer"
                           :description "Legacy no-id task")
                         "rendered legacy body\n"
                         nil)))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering source)
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                   (lambda (buf start end &optional _collapsed-only)
                     (should (eq buf data-buf))
                     (should (= start (car source)))
                     (should (= end (cdr source)))
                     rendering)))
          (goto-char (point-min))
          (search-forward "Agent: explorer")
          (goto-char (match-beginning 0))
          (should (eq (get-text-property (point) 'mevedel-view-type)
                      'agent-handle))
          (should (eq (get-text-property (point)
                                         'mevedel-view-collapsed)
                      t))
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rendered legacy body" text))
            (should-not (string-match-p "raw launch payload" text)))
          (goto-char (point-min))
          (search-forward "Agent: explorer")
          (goto-char (match-beginning 0))
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "rendered legacy body" text)))))))

  :doc "agent handles with unopenable ids still expand inline"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
    (with-current-buffer view-buf
      (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
             (rendering (mevedel-tool-ui--render-agent
                         "Agent"
                         '(:subagent_type "explorer"
                           :description "Malformed render-data task")
                         "rendered malformed body\n"
                         '(:kind something-else
                           :agent-id "explorer--badid"))))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering source)
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                   (lambda (_buf _start _end) rendering)))
          (goto-char (point-min))
          (search-forward "Agent: explorer")
          (goto-char (match-beginning 0))
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rendered malformed body" text))
            (should-not (string-match-p "raw launch payload" text)))))))

  :doc "agent handles with missing saved transcripts report unavailable"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--missing")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "missing-transcript"
                       :root temporary-file-directory
                       :name "missing-transcript"))
           (session (mevedel-session-create "main" workspace))
           opened
           message-text)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (rendering (mevedel-tool-ui--render-agent
                           "Agent"
                           '(:subagent_type "explorer"
                             :description "Missing transcript task")
                           "rendered missing transcript body\n"
                           `(:kind agent-transcript
                             :agent-id ,agent-id
                             :status completed))))
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t)
            (unwind-protect
                (mevedel-view--insert-rendered-tool rendering source)
              (set-marker-insertion-type mevedel-view--input-marker nil)))
          (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                     (lambda (_buf _start _end) rendering))
                    ((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (_id)
                       (setq opened t)
                       (user-error "Transcript file missing")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (goto-char (point-min))
            (search-forward "Agent: explorer")
            (goto-char (match-beginning 0))
            (mevedel-view-toggle-section)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should opened)
              (should (equal "Transcript file missing" message-text))
              (should-not (string-match-p
                           "rendered missing transcript body" text))
              (should-not (string-match-p "raw launch payload" text))))))))

  :doc "agent handles with stale live invocations report unavailable"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--stale")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "stale-live-transcript"
                       :root temporary-file-directory
                       :name "stale-live-transcript"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'running))
           opened
           message-text)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
      (with-current-buffer view-buf
        (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
               (rendering (mevedel-tool-ui--render-agent
                           "Agent"
                           '(:subagent_type "explorer"
                             :description "Stale live task")
                           "rendered stale live body\n"
                           `(:kind agent-transcript
                             :agent-id ,agent-id
                             :status running))))
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (set-marker-insertion-type mevedel-view--input-marker t)
            (unwind-protect
                (mevedel-view--insert-rendered-tool rendering source)
              (set-marker-insertion-type mevedel-view--input-marker nil)))
          (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                     (lambda (_buf _start _end) rendering))
                    ((symbol-function 'mevedel-view--agent-invocation)
                     (lambda (_id) inv))
                    ((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (_id)
                       (setq opened t)
                       (user-error "Live buffer unavailable")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (goto-char (point-min))
            (search-forward "Agent: explorer")
            (goto-char (match-beginning 0))
            (mevedel-view-toggle-section)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should opened)
              (should (equal "Live buffer unavailable" message-text))
              (should-not (string-match-p "rendered stale live body" text))
              (should-not (string-match-p "raw launch payload" text))))))))

(mevedel-deftest mevedel-tool-ui--render-agent-body
  (:doc "selects the correct Agent expanded body for foreground and background rows")
  ,test
  (test)

  :doc "agent description truncation never exceeds narrow widths"
  (progn
    (should (equal "" (mevedel-tool-ui--compact-agent-description
                       "long task" 0)))
    (should (equal "." (mevedel-tool-ui--compact-agent-description
                       "long task" 1)))
    (should (equal ".." (mevedel-tool-ui--compact-agent-description
                        "long task" 2)))
    (should (equal "..." (mevedel-tool-ui--compact-agent-description
                         "long task" 3)))
    (should (<= (string-width
                 (mevedel-tool-ui--compact-agent-description
                  "long task" 2))
                2)))

  :doc "foreground Agent rows still render the final response"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--fg"
                 :status completed
                 :activity ((:type tool-start :tool-name "Read"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "final response body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--render-expanded-body rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "final response body" text))
          (should-not (string-match-p "Read" text)))))))

  :doc "Agent rows render SubagentStart hook context as a compact audit note"
  (mevedel-view-test--with-buffers
    (let* ((args '(:subagent_type "explorer" :description "Task"))
           (rd '(:kind agent-transcript
                 :agent-id "explorer--hook"
                 :status running
                 :hook-audits
                 ((:type subagent-context
                   :event "SubagentStart"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args "launch body" rd)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--insert-rendered-tool rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "hook added sub-agent context" text))
          (should-not (string-match-p "extra start context" text))))))

  :doc "Agent handle transcript click target is the visible type label"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "agent-target"
                       :root temporary-file-directory
                       :name "agent-target"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-agent-target-session")))
           (args '(:subagent_type "explorer" :description "Task"))
           (rd `(:kind agent-transcript
                 :agent-id ,agent-id
                 :status completed))
           rendering)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq rendering (mevedel-tool-ui--render-agent
                         "Agent" args "final response body" rd))
        (let ((inhibit-read-only t)
              start)
          (goto-char mevedel-view--input-marker)
          (setq start (point))
          (mevedel-view--insert-rendered-tool rendering (cons 1 1))
          (mevedel-view--add-display-region-properties
           start (point) 'agent-handle)
          (should
           (string-search
            "Agent: explorer -- Task"
            (buffer-substring-no-properties start (point))))
          (goto-char start)
          (search-forward "Agent: explorer")
          (search-backward "explorer")
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-label-map))
          (should (equal agent-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should
           (lookup-key (get-text-property (point) 'keymap) [mouse-1])))))

  :doc "Agent handle header normalizes long task text to one line"
  (let* ((args '(:subagent_type "coordinator"
                 :description "Run validation.\nThen repeat until green."))
         (rd '(:kind agent-transcript
               :agent-id "coordinator--abcdef123456"
               :status running
               :calls 9))
         (mevedel-tool-ui-agent-description-width 30)
         (rendering (mevedel-tool-ui--render-agent
                     "Agent" args "launch status" rd))
         (header (plist-get rendering :header)))
    (should (string-match-p
             "Agent: coordinator -- Run validation\\. Then repeat\\.\\.\\."
             header))
    (should-not (string-match-p "\n" header))
    (should (string-match-p "\\[running · 9 calls\\]" header))))

(mevedel-deftest mevedel-view--linkify-paths-in-range ()
  ,test
  (test)
  :doc "slashless root filename is buttonized when it exists"
  (let* ((root (make-temp-file "mevedel-view-linkify-" t))
         (file (file-name-concat root "mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-root"
                     :root root :name "linkify-root"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "Read: mevedel-skills.el\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "mevedel-skills.el")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path))))))
      (delete-directory root t)))

  :doc "missing slashless filename stays plain text"
  (let* ((root (make-temp-file "mevedel-view-linkify-missing-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-missing"
                     :root root :name "linkify-missing"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (insert "Read: missing-file.el\n")
          (mevedel-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward "missing-file.el")
          (should-not (button-at (match-beginning 0))))
      (delete-directory root t)))

  :doc "slash-containing relative path is still buttonized"
  (let* ((root (make-temp-file "mevedel-view-linkify-rel-" t))
         (file (file-name-concat root "test/test-mevedel-skills.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-rel"
                     :root root :name "linkify-rel"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "subdir\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "Edit: test/test-mevedel-skills.el\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "test/test-mevedel-skills.el")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path))))))
      (delete-directory root t)))

  :doc "URL-like text is not buttonized"
  (let* ((root (make-temp-file "mevedel-view-linkify-url-" t))
         (file (file-name-concat root "example.com"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-url"
                     :root root :name "linkify-url"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "not a link target here\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See https://example.com\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "example.com")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t)))

  :doc "relative file line reference stores path and line"
  (let* ((root (make-temp-file "mevedel-view-linkify-line-" t))
         (file (file-name-concat root "mevedel-session-persistence.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-line"
                     :root root :name "linkify-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See mevedel-session-persistence.el:187\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "mevedel-session-persistence.el:187")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 187 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "relative file line range stores first line and spans full range"
  (let* ((root (make-temp-file "mevedel-view-linkify-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-range"
                     :root root :name "linkify-range"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:100-102\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:100-102")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 100 (button-get button 'mevedel-view-line)))
              (let ((end-button (button-at (1- (match-end 0)))))
                (should end-button)
                (should (equal file
                               (button-get end-button 'mevedel-view-path)))
                (should (= 100
                           (button-get end-button 'mevedel-view-line)))))))
      (delete-directory root t)))

  :doc "relative file L-prefixed line range stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-l-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-l-range"
                     :root root :name "linkify-l-range"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:L1400-L1422\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:L1400-L1422")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 1400 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "colon line list creates separate buttons"
  (let* ((root (make-temp-file "mevedel-view-linkify-list-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-list"
                     :root root :name "linkify-list"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:L24,L120-L143\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:L24")
            (let ((first (button-at (match-beginning 0))))
              (should first)
              (should (= 24 (button-get first 'mevedel-view-line))))
            (search-forward ",")
            (should-not (button-at (1- (point))))
            (search-forward "L120-L143")
            (let ((second (button-at (match-beginning 0))))
              (should second)
              (should (equal file
                             (button-get second 'mevedel-view-path)))
              (should (= 120 (button-get second 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "hash line list creates separate buttons"
  (let* ((root (make-temp-file "mevedel-view-linkify-hash-list-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-hash-list"
                     :root root :name "linkify-hash-list"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:#L24,#L120-#L143\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:#L24")
            (let ((first (button-at (match-beginning 0))))
              (should first)
              (should (= 24 (button-get first 'mevedel-view-line))))
            (search-forward ",")
            (should-not (button-at (1- (point))))
            (search-forward "#L120-#L143")
            (let ((second (button-at (match-beginning 0))))
              (should second)
              (should (equal file
                             (button-get second 'mevedel-view-path)))
              (should (= 120 (button-get second 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "direct #L fragment stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-hash-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-hash"
                     :root root :name "linkify-hash"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el#L24-L30\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el#L24-L30")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 24 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "@file mention with line reference stores path and line"
  (let* ((root (make-temp-file "mevedel-view-linkify-file-mention-" t))
         (file (file-name-concat root "with space.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-file-mention"
                     :root root :name "linkify-file-mention"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert (format "See @file:{%s}#L7\n" file))
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "@file:")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 7 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "Markdown local link #L range stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-md-range-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-md-range"
                     :root root :name "linkify-md-range"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "[spot](file.el#L24-L30)\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "spot")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 24 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "Markdown local link colon line suffix stores first line"
  (let* ((root (make-temp-file "mevedel-view-linkify-md-colon-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-md-colon"
                     :root root :name "linkify-md-colon"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "[plain](file.el:24)\n[prefixed](file.el:L25-L30)\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "plain")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 24 (button-get button 'mevedel-view-line))))
            (search-forward "prefixed")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 25 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "nested relative file line reference resolves from workspace root"
  (let* ((root (make-temp-file "mevedel-view-linkify-nested-line-" t))
         (file (file-name-concat root "test/test-mevedel-agent-exec.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-nested-line"
                     :root root :name "linkify-nested-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "nested\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See test/test-mevedel-agent-exec.el:803\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "test/test-mevedel-agent-exec.el:803")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (equal file
                             (button-get button 'mevedel-view-path)))
              (should (= 803 (button-get button 'mevedel-view-line))))))
      (delete-directory root t)))

  :doc "absolute file line reference stores path and line"
  (let ((file (make-temp-file "mevedel-view-linkify-abs-" nil ".el")))
    (unwind-protect
        (with-temp-buffer
          (insert "See " file ":42\n")
          (mevedel-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward (concat file ":42"))
          (let ((button (button-at (match-beginning 0))))
            (should button)
            (should (equal file
                           (button-get button 'mevedel-view-path)))
            (should (= 42 (button-get button 'mevedel-view-line)))))
      (delete-file file)))

  :doc "activating file line reference jumps to the requested line"
  (let ((file (make-temp-file "mevedel-view-linkify-action-" nil ".el"))
        opened)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "one\ntwo\nthree\nfour\n"))
          (with-temp-buffer
            (insert "See " file ":3\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward (concat file ":3"))
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (cl-letf (((symbol-function 'find-file-other-window)
                         (lambda (path)
                           (setq opened (find-file-noselect path)))))
                (button-activate button))))
          (should (buffer-live-p opened))
          (with-current-buffer opened
            (should (= 3 (line-number-at-pos)))
            (should (looking-at "three"))))
      (when (buffer-live-p opened)
        (kill-buffer opened))
      (delete-file file)))

  :doc "missing file line reference stays plain text"
  (let* ((root (make-temp-file "mevedel-view-linkify-missing-line-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-missing-line"
                     :root root :name "linkify-missing-line"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (insert "See missing-file.el:10\n")
          (mevedel-view--linkify-paths-in-range (point-min) (point-max))
          (goto-char (point-min))
          (search-forward "missing-file.el:10")
          (should-not (button-at (match-beginning 0))))
      (delete-directory root t)))

  :doc "URL-like text with port is not buttonized"
  (with-temp-buffer
    (insert "See https://example.com:443\n")
    (mevedel-view--linkify-paths-in-range (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "example.com:443")
    (should-not (button-at (match-beginning 0))))

  :doc "trailing punctuation is not part of a line reference button"
  (let* ((root (make-temp-file "mevedel-view-linkify-punct-" t))
         (file (file-name-concat root "file.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "linkify-punct"
                     :root root :name "linkify-punct"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "punct\n"))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (insert "See file.el:42.\n")
            (mevedel-view--linkify-paths-in-range (point-min) (point-max))
            (goto-char (point-min))
            (search-forward "file.el:42")
            (let ((button (button-at (match-beginning 0))))
              (should button)
              (should (= 42 (button-get button 'mevedel-view-line)))
              (should-not (button-at (point))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view--render-tool-group/fallback-linkifies-paths ()
  ,test
  (test)
  :doc "fallback one-liner buttonizes existing file paths"
  (let* ((root (make-temp-file "mevedel-view-fallback-linkify-" t))
         (file (file-name-concat root "mevedel-tool-plan.el"))
         (workspace (mevedel-workspace--create
                     :type 'project :id "fallback-linkify"
                     :root root :name "fallback-linkify"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "root\n"))
          (mevedel-view-test--with-buffers
            (with-current-buffer data-buf
              (erase-buffer)
              (insert "(:name \"Edit\" :args (:file_path \"mevedel-tool-plan.el\"))\n"
                      "Error: nope\n"))
            (with-current-buffer view-buf
              (setq-local mevedel--session session)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                           (lambda (&rest _) nil)))
                  (mevedel-view--render-tool-group
                   (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
                   data-buf)))
              (goto-char (point-min))
              (should (search-forward "! Edit: mevedel-tool-plan.el (1 lines)" nil t))
              (goto-char (point-min))
              (search-forward "mevedel-tool-plan.el")
              (let ((button (button-at (match-beginning 0))))
                (should button)
                (should (equal file
                               (button-get button 'mevedel-view-path)))))))
      (delete-directory root t)))

  :doc "fallback one-liner leaves missing file paths plain"
  (let* ((root (make-temp-file "mevedel-view-fallback-missing-" t))
         (workspace (mevedel-workspace--create
                     :type 'project :id "fallback-missing"
                     :root root :name "fallback-missing"))
         (session (mevedel-session-create "main" workspace)))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (erase-buffer)
            (insert "(:name \"Edit\" :args (:file_path \"missing-file.el\"))\n"
                    "Error: nope\n"))
          (with-current-buffer view-buf
            (setq-local mevedel--session session)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                         (lambda (&rest _) nil)))
                (mevedel-view--render-tool-group
                 (list (list 'tool 1 (with-current-buffer data-buf (point-max))))
                 data-buf)))
            (goto-char (point-min))
            (should (search-forward "! Edit: missing-file.el (1 lines)" nil t))
            (goto-char (point-min))
            (search-forward "missing-file.el")
            (should-not (button-at (match-beginning 0)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view--render-tool-group/source-collapse-state ()
  ,test
  (test)
  :doc "expanded saved state renders full body instead of collapsed cache"
  (mevedel-view-test--with-buffers
    (let (source)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "CacheStateTool"
        :category "mevedel"
        :renderer (lambda (_name _args result _data)
                    (list :header "CacheStateTool: cached"
                          :body result
                          :body-mode 'text-mode
                          :initially-collapsed-p t))))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"CacheStateTool\" :args (:path \"cached\"))\n\nbody must survive cache\n"
       '(tool . "call_cache_state"))
      (setq source (cons 1 (with-current-buffer data-buf (point-max))))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (mevedel-view--render-tool-group
           (list (list 'tool (car source) (cdr source))) data-buf))
        (goto-char (point-min))
        (search-forward "CacheStateTool: cached")
        (let ((header-pos (match-beginning 0)))
          (should-not (search-forward "body must survive cache"
                                      mevedel-view--input-marker t))
          (goto-char header-pos))
        (mevedel-view-toggle-section)
        (should (search-forward "body must survive cache"
                                mevedel-view--input-marker t))
        (let ((inhibit-read-only t))
          (delete-region (point-min) mevedel-view--input-marker)
          (goto-char mevedel-view--input-marker)
          (mevedel-view--render-tool-group
           (list (list 'tool (car source) (cdr source))) data-buf))
        (goto-char (point-min))
        (search-forward "CacheStateTool: cached")
        (should-not (get-text-property (match-beginning 0)
                                       'mevedel-view-collapsed))
        (should (search-forward "body must survive cache"
                                mevedel-view--input-marker t))))))

(mevedel-deftest mevedel-view--rendering-header-face
  (:doc "selects distinct faces for agent handle header states")
  ,test
  (test)

  :doc "running agent handles use the active running face"
  (should (eq 'mevedel-view-agent-running
              (mevedel-view--rendering-header-face
               '(:vtype agent-handle :agent-status running))))

  :doc "completed agent handles use the normal tool summary face"
  (should (eq 'mevedel-view-tool-summary
              (mevedel-view--rendering-header-face
               '(:vtype agent-handle :agent-status completed))))

  :doc "ordinary tool rows use the normal tool summary face"
  (should (eq 'mevedel-view-tool-summary
              (mevedel-view--rendering-header-face
               '(:vtype tool-summary)))))

(mevedel-deftest mevedel-view--rendering-header-line
  (:doc "styles renderer-provided tool headers like normal tool summaries")
  ,test
  (test)

  :doc "tool name and argument get distinct faces"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary :header "ToolSearch: Eval"))))
    (should (string-match "ToolSearch" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "Eval" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "trailing line counts get the metadata face"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary
                 :header "Read: mevedel-tools.el (95 lines)"))))
    (should (string-match "Read" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "mevedel-tools.el" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(95 lines)" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "trailing match counts get the metadata face"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary
                 :header
                 "Grep: sanitize-gptel-bounds|GPTEL_BOUNDS (720 matches)"))))
    (should (string-match "Grep" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "sanitize-gptel-bounds|GPTEL_BOUNDS" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(720 matches)" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "diff count metadata keeps added and removed counts distinct"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype tool-summary
                 :header "Edit: mevedel-view.el (+1 -0)"))))
    (should (string-match "Edit" line))
    (should (eq 'mevedel-view-tool-name
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "mevedel-view.el" line))
    (should (eq 'mevedel-view-tool-argument
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "(" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "\\+1" line))
    (should (eq 'mevedel-view-tool-diff-added
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))
    (should (string-match "-0" line))
    (should (eq 'mevedel-view-tool-diff-removed
                (get-text-property (match-beginning 0)
                                   'font-lock-face line))))

  :doc "incomplete agent handles do not use a success marker"
  (let ((line (mevedel-view--rendering-header-line
               '(:vtype agent-handle
                 :agent-status incomplete
                 :header "Agent: verifier"))))
    (should (string-match "…" line))
    (should (eq 'mevedel-view-tool-metadata
                (get-text-property (match-beginning 0)
                                   'font-lock-face line)))))

(mevedel-deftest mevedel-view--section-bounds ()
  ,test
  (test)
  :doc "distinguishes equal-but-distinct source conses (regression)
Thinking-cons and turn-fallback-cons can have equal values but be
separate cons objects.  `section-bounds' must compare by `eq', not
`equal', or it will treat them as one run and expand/collapse over
the preceding header."
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "deep thoughts here\n"))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            ;; Equal values, distinct objects — matches the real render
            ;; path where the thinking summary's source cons and the
            ;; turn-level fallback cons may print identically.
            (thinking-src (cons 1 20))
            (turn-src (cons 1 20)))
        (should (equal thinking-src turn-src))
        (should-not (eq thinking-src turn-src))
        (save-excursion
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((insert-start (point)))
                (insert (propertize "Assistant\n"
                                    'font-lock-face 'mevedel-view-assistant-header))
                (insert (propertize "Thinking... (1 lines)\n"
                                    'font-lock-face 'mevedel-view-thinking-summary
                                    'mevedel-view-type 'thinking-summary
                                    'mevedel-view-collapsed t
                                    'mevedel-view-source thinking-src))
                (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
                (add-text-properties insert-start (point) '(read-only t))
                ;; Fill the header/separator gap with the turn-level
                ;; fallback cons, as `mevedel-view--render-turn' does.
                (let ((pos insert-start))
                  (while (< pos (point))
                    (if (get-text-property pos 'mevedel-view-source)
                        (setq pos (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point)))
                      (let ((next (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point))))
                        (put-text-property pos next
                                           'mevedel-view-source turn-src)
                        (setq pos next))))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        ;; Point at the exact start of the thinking run — the boundary
        ;; case where `previous-single-property-change' lands in the
        ;; preceding "Assistant\n" run.
        (goto-char (point-min))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (let ((bounds (mevedel-view--section-bounds)))
          (should bounds)
          (should (eq (get-text-property (car bounds)
                                         'mevedel-view-source)
                      thinking-src))
          (should (eq (get-text-property (point) 'mevedel-view-source)
                      thinking-src))
          ;; The bounds must not reach into the Assistant header.
          (let ((header-text (buffer-substring-no-properties
                              (car bounds) (cdr bounds))))
            (should-not (string-match-p "Assistant" header-text))))))))

(mevedel-deftest mevedel-view-toggle-section/thinking-preserves-headers ()
  ,test
  (test)
  :doc "TAB expand then collapse on thinking keeps surrounding
headers intact (regression for the \"You/Assistant disappear\" bug
when thinking-cons and turn-cons had equal-but-distinct values)."
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "deep thoughts here\n"))
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (thinking-src (cons 1 20))
            (turn-src (cons 1 20)))
        ;; User section (no source, mirrors `--insert-user-message').
        (save-excursion
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert (propertize "You\n"
                                    'font-lock-face 'mevedel-view-user-header))
                (insert "Think about it.\n")
                (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
                (add-text-properties start (point)
                                     '(read-only t mevedel-view-type user)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        ;; Assistant turn: header + thinking summary + separator, with
        ;; the turn-level fallback source equal-but-not-eq to thinking.
        (save-excursion
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((insert-start (point)))
                (insert (propertize "Assistant\n"
                                    'font-lock-face 'mevedel-view-assistant-header))
                (insert (propertize "Thinking... (1 lines)\n"
                                    'font-lock-face 'mevedel-view-thinking-summary
                                    'mevedel-view-type 'thinking-summary
                                    'mevedel-view-collapsed t
                                    'mevedel-view-source thinking-src))
                (insert (propertize "\n" 'font-lock-face 'mevedel-view-separator))
                (add-text-properties insert-start (point) '(read-only t))
                (let ((pos insert-start))
                  (while (< pos (point))
                    (if (get-text-property pos 'mevedel-view-source)
                        (setq pos (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point)))
                      (let ((next (or (next-single-property-change
                                       pos 'mevedel-view-source nil (point))
                                      (point))))
                        (put-text-property pos next
                                           'mevedel-view-source turn-src)
                        (setq pos next))))))
            (set-marker-insertion-type mevedel-view--input-marker nil))))
      ;; Point at the exact start of the thinking line.
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      ;; Expand.
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^You$" text))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "deep thoughts here" text))
        (should-not (string-match-p "^Thinking\\.\\.\\." text)))
      ;; Collapse back — the thinking summary must return and headers
      ;; must still be intact.
      (goto-char (point-min))
      (search-forward "deep thoughts here")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^You$" text))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "Thinking\\.\\.\\. (1 lines)" text))
        (should-not (string-match-p "deep thoughts here" text))))))

(mevedel-deftest mevedel-view-toggle-section/response ()
  ,test
  (test)
  :doc "response text is a collapsible section (regression for
earlier removal of the catch-all collapse branch that dropped
response folding along with a dangerous best-guess preview path)."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "First line of reply.\nSecond line.\nThird line.\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "First line of reply")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'response))
      (should (eq (get-text-property (point) 'mevedel-view-collapsed) nil))
      ;; Collapse.
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "First line of reply" text))
        (should-not (string-match-p "Second line" text))
        (should (string-match-p "(3 lines)" text)))
      ;; Expand back.
      (goto-char (point-min))
      (search-forward "First line of reply")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Second line" text))
        (should (string-match-p "Third line" text)))))

  :doc "response collapse and expand keep known proposed-plan blocks hidden"
  (mevedel-view-test--with-buffers
    (let* ((old-plan "# Hidden plan\n")
           (session (mevedel-session--create
                     :name "test"
                     :workspace nil
                     :permission-mode 'default
                     :plan-metadata
                     (list :presented-plan-hashes
                           (list (mevedel-plan-mode--plan-hash old-plan))))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data
       data-buf
       (concat "Visible lead.\n<proposed_plan>\n"
               old-plan
               "</proposed_plan>\nVisible tail.\n")
       'response)
      (with-current-buffer data-buf
        (mevedel-view--render-response (point-min) (point-max)))
      (with-current-buffer view-buf
        (goto-char (point-min))
        (search-forward "Visible lead")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (let ((collapsed (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
          (should-not (string-match-p "<proposed_plan>" collapsed))
          (should-not (string-match-p "# Hidden plan" collapsed)))
        (goto-char (point-min))
        (search-forward "Visible lead")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (let ((expanded (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Visible tail" expanded))
          (should-not (string-match-p "<proposed_plan>" expanded))
          (should-not (string-match-p "# Hidden plan" expanded))))))

  :doc "response table collapse and expand does not leave duplicate rows"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "| Name | Role |\n|------|------|\n| Alice | Engineer |\n| Bob | Designer |\n| Carol | Product Manager |\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "Alice")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((collapsed (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
        (should (string-match-p "(5 lines)" collapsed))
        (should-not (string-match-p "^| Bob" collapsed)))
      (goto-char (point-min))
      (search-forward "Name | Role")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((expanded (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
        (should (= 1 (mevedel-view-test--count-substring "Alice" expanded)))
        (should (= 1 (mevedel-view-test--count-substring "Bob" expanded)))
        (should (= 1 (mevedel-view-test--count-substring "Carol" expanded)))))))

(mevedel-deftest mevedel-view-toggle-section/assistant-turn ()
  ,test
  (test)
  :doc "TAB on Assistant header folds the whole turn into a single
summary line; TAB again restores it exactly."
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hi\n" nil)
    (mevedel-view-test--insert-data
     data-buf
     "Here is the first line.\nSecond line.\nThird line.\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "Assistant")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'turn-header))
      ;; Fold the turn.
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Assistant — Here is the first line" text))
        (should-not (string-match-p "^Second line" text))
        (should-not (string-match-p "^Third line" text))
        ;; The user turn is untouched.
        (should (string-match-p "^You$" text))
        (should (string-match-p "^Hi$" text)))
      ;; Expand back.
      (goto-char (point-min))
      (search-forward "Assistant — ")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'turn-summary))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "Here is the first line" text))
        (should (string-match-p "Second line" text))
        (should (string-match-p "Third line" text))))))

(mevedel-deftest mevedel-view-toggle-section/user-turn ()
  ,test
  (test)
  :doc "multi-line user turn folds to first-line summary
Single-line user turns refuse to fold since they are already
compact."
  (mevedel-view-test--with-buffers
    ;; Multi-line user turn.
    (mevedel-view-test--insert-data
     data-buf
     "*** First prompt line.\nSecond prompt line.\nThird prompt line.\n"
     nil)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "You")
      (goto-char (match-beginning 0))
      (should (eq (get-text-property (point) 'mevedel-view-type) 'turn-header))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "First prompt line" text))
        (should-not (string-match-p "^Second prompt line" text))
        (should (string-match-p "(3 lines)" text)))
      ;; Expand.
      (goto-char (point-min))
      (search-forward "First prompt line")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^You$" text))
        (should (string-match-p "Second prompt line" text))
        (should (string-match-p "Third prompt line" text)))))
  :doc "single-line user turn refuses to fold"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** One line only.\n" nil)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (goto-char (point-min))
      (search-forward "You")
      (goto-char (match-beginning 0))
      (should-error (mevedel-view-toggle-section)
                    :type 'user-error))))

(mevedel-deftest mevedel-view-toggle-section/turn-preserves-inner-state ()
  ,test
  (test)
  :doc "folding and unfolding a turn preserves the expanded/collapsed
state of its inner sections"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "deep thoughts live here\n")
        (put-text-property start (point) 'gptel 'ignore))
      (let ((start (point)))
        (insert "Visible response text.\n")
        (put-text-property start (point) 'gptel 'response))
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      ;; Expand the thinking section first.
      (goto-char (point-min))
      (search-forward "Thinking...")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "deep thoughts live here" text))
        (should-not (string-match-p "Thinking\\.\\.\\." text)))
      ;; Fold the whole turn.
      (goto-char (point-min))
      (search-forward "Assistant")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Assistant — " text))
        (should-not (string-match-p "deep thoughts live here" text)))
      ;; Unfold — the thinking section must still be EXPANDED.
      (goto-char (point-min))
      (search-forward "Assistant — ")
      (goto-char (match-beginning 0))
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "^Assistant$" text))
        (should (string-match-p "deep thoughts live here" text))
        (should-not (string-match-p "Thinking\\.\\.\\." text))
        (should (string-match-p "Visible response text" text))))))

(mevedel-deftest mevedel-view-collapse-state-survives-streaming ()
  ,test
  (test)
  :doc "expanded renderer-backed tool survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "StateTool"
        :category "mevedel"
        :renderer (lambda (_name _args result _data)
                    (list :header "StateTool: a.txt"
                          :body result
                          :body-mode 'text-mode
                          :initially-collapsed-p t))))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "Intro.\n" 'response)
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"StateTool\" :args (:path \"a.txt\"))\n\nexpanded tool body\n"
       '(tool . "call_state_tool"))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "StateTool: a.txt")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "expanded tool body"
                                mevedel-view--input-marker t)))
      (mevedel-view-test--insert-data data-buf "Stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "expanded tool body" text))
          (should (string-match-p "Stream tail" text))))))

  :doc "expanded short thinking survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "short thought\n" 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "Thinking...")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "short thought"
                                mevedel-view--input-marker t)))
      (mevedel-view-test--insert-data data-buf "more streamed thinking\n" 'ignore)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "short thought" text))
          (should (string-match-p "more streamed thinking" text))
          (should-not (string-match-p "Thinking\\.\\.\\." text)))
        (when (markerp mevedel-view--data-turn-start)
          (set-marker mevedel-view--data-turn-start nil))
        (setq mevedel-view--data-turn-start nil)
        (when (markerp mevedel-view--in-flight-turn-start)
          (set-marker mevedel-view--in-flight-turn-start nil))
        (setq mevedel-view--in-flight-turn-start nil)
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "short thought" text))
          (should (string-match-p "more streamed thinking" text))
          (should-not (string-match-p "Thinking\\.\\.\\." text))))))

  :doc "expanded source-backed agent handle survives in-flight incremental render"
  (mevedel-view-test--with-buffers
    (let (assistant-start view-assistant-start)
      (mevedel-tool-register
       (mevedel-tool--create
        :name "StateAgent"
        :category "mevedel"
        :renderer (lambda (_name _args result _data)
                    (list :header "Agent: verifier -- check state"
                          :body result
                          :body-mode 'text-mode
                          :vtype 'agent-handle
                          :initially-collapsed-p t))))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"StateAgent\" :args (:subagent_type \"verifier\"))\n\nagent body stays open\n"
       '(tool . "call_state_agent"))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil))
        (search-forward "Agent: verifier -- check state")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (should (search-forward "agent body stays open"
                                mevedel-view--input-marker t)))
      (mevedel-view-test--insert-data data-buf "Agent stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "agent body stays open" text))
          (should (string-match-p "Agent stream tail" text))))))

  :doc "expanded agent-result mailbox card survives in-flight incremental render"
  (let ((mevedel-view-mailbox-collapse-line-threshold 1))
    (mevedel-view-test--with-buffers
      (let (assistant-start view-assistant-start)
        (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
        (with-current-buffer data-buf
          (setq assistant-start (copy-marker (point) nil)))
        (mevedel-view-test--insert-data data-buf "Before result.\n" 'response)
        (mevedel-view-test--insert-data
         data-buf
         "\n<agent-result agent-id=\"worker--state\" type=\"worker\">\nline one\nline two\n</agent-result>\n\n"
         nil)
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (setq mevedel-view--data-turn-start assistant-start)
          (goto-char (point-min))
          (search-forward "Assistant")
          (setq view-assistant-start (match-beginning 0))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker view-assistant-start nil))
          (search-forward "worker--state")
          (goto-char (match-beginning 0))
          (mevedel-view-toggle-section)
          (goto-char (point-min))
          (search-forward "line two")
          (should-not (get-text-property (match-beginning 0) 'invisible)))
        (mevedel-view-test--insert-data data-buf "Result stream tail.\n" 'response)
        (with-current-buffer view-buf
          (mevedel-view--render-incremental data-buf)
          (goto-char (point-min))
          (search-forward "line two")
          (should-not (get-text-property (match-beginning 0) 'invisible))
          (should (search-forward "Result stream tail"
                                  mevedel-view--input-marker t))))))

  :doc "expanded task status survives streaming redraw and full rerender"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "task-state-stream"
                       :root temporary-file-directory
                       :name "task-state-stream"))
           (session (mevedel-session-create "main" workspace))
           assistant-start view-assistant-start)
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "active detail" :status 'pending)
                  (mevedel-task--create
                   :id 2 :subject "finished detail" :status 'completed)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--view-buffer view-buf))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq assistant-start (copy-marker (point) nil)))
      (mevedel-view-test--insert-data data-buf "Task response.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "active detail")
        (mevedel-toggle-tasks)
        (should (search-forward "finished detail"
                                mevedel-view--input-marker t))
        (setq mevedel-view--data-turn-start assistant-start)
        (goto-char (point-min))
        (search-forward "Assistant")
        (setq view-assistant-start (match-beginning 0))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker view-assistant-start nil)))
      (mevedel-view-test--insert-data data-buf "Task stream tail.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--render-incremental data-buf)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "finished detail" text))
          (should (string-match-p "Task stream tail" text)))
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (should (search-forward "finished detail"
                                mevedel-view--input-marker t))))))


;;
;;; Rendering plist validation

(mevedel-deftest mevedel-view--rendering-plist-p ()
  ,test
  (test)
  :doc "accepts minimal plist with just :header"
  (should (mevedel-view--rendering-plist-p '(:header "h")))
  :doc "accepts full plist with string body and symbol mode"
  (should (mevedel-view--rendering-plist-p
           '(:header "h" :body "b" :body-mode diff-mode
                     :initially-collapsed-p t)))
  :doc "accepts status and non-expandable marker"
  (should (mevedel-view--rendering-plist-p
           '(:header "h" :status error :expandable-p nil)))
  :doc "rejects missing :header"
  (should-not (mevedel-view--rendering-plist-p '(:body "b")))
  :doc "rejects non-string :header"
  (should-not (mevedel-view--rendering-plist-p '(:header 42)))
  :doc "rejects non-string :body"
  (should-not (mevedel-view--rendering-plist-p '(:header "h" :body 42)))
  :doc "rejects non-symbol :body-mode"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :body-mode "not-a-symbol")))
  :doc "rejects non-symbol :status"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :status "error")))
  :doc "rejects non-boolean :expandable-p"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :expandable-p maybe))))


;;
;;; Renderer invocation

(mevedel-deftest mevedel-view--invoke-renderer ()
  ,test
  (test)
  :doc "returns the renderer's plist on success"
  (let* ((tool (mevedel-tool--create
                :name "R1"
                :renderer (lambda (_name _args _result _data)
                            (list :header "ok"
                                  :body "b"
                                  :body-mode 'diff-mode)))))
    (should (equal '(:header "ok" :body "b" :body-mode diff-mode)
                   (mevedel-view--invoke-renderer
                    tool '(:kind diff) nil "result"))))
  :doc "invokes the renderer even when render-data is nil (output-driven renderers)"
  (let ((tool (mevedel-tool--create
               :name "R2"
               :renderer (lambda (_name _args _result _data)
                           (list :header "x")))))
    (should (equal '(:header "x")
                   (mevedel-view--invoke-renderer tool nil nil "ok"))))

  :doc "data-driven renderers can opt out by returning nil when render-data is absent"
  (let ((tool (mevedel-tool--create
               :name "R2-data"
               :renderer (lambda (_name _args _result data)
                           (and data (list :header "only with data"))))))
    (should (null (mevedel-view--invoke-renderer tool nil nil "ok"))))
  :doc "renderer alist dispatches by success status"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (mevedel-tool--create
                :name "StatusDispatch"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "success")
                   (mevedel-view--invoke-renderer tool nil nil "ok"))))
  :doc "renderer alist dispatches by error status"
  (let* ((success-fn (lambda (_name _args _result _data)
                       (list :header "success")))
         (error-fn (lambda (_name _args _result _data)
                     (list :header "error")))
         (tool (mevedel-tool--create
                :name "StatusDispatchErr"
                :renderer `((success . ,success-fn)
                            (error . ,error-fn)))))
    (should (equal '(:header "error")
                   (mevedel-view--invoke-renderer
                    tool nil nil "Error: bad"))))
  :doc "renderer alist falls back to default status"
  (let* ((default-fn (lambda (_name _args _result _data)
                       (list :header "default")))
         (tool (mevedel-tool--create
                :name "StatusDefault"
                :renderer `((default . ,default-fn)))))
    (should (equal '(:header "default")
                   (mevedel-view--invoke-renderer
                    tool nil nil "Error: bad"))))
  :doc "returns nil when tool has no renderer"
  (let ((tool (mevedel-tool--create :name "NoRend" :renderer nil)))
    (should (null (mevedel-view--invoke-renderer
                   tool '(:kind diff) nil "ok"))))
  :doc "renderer returning nil yields nil (silent fallback)"
  (let ((tool (mevedel-tool--create
               :name "Declines"
               :renderer (lambda (_name _args _result _data) nil))))
    (should (null (mevedel-view--invoke-renderer
                   tool '(:kind diff) nil "ok"))))
  :doc "renderer returning malformed plist yields nil and emits a warning"
  (let* ((tool (mevedel-tool--create
                :name "Bad"
                :renderer (lambda (_name _args _result _data)
                            '(:body "no header"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (mevedel-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'mevedel (caar warnings)))
      (should (string-match-p "malformed" (cadar warnings)))))
  :doc "renderer signalling an error yields nil and emits a warning"
  (let* ((tool (mevedel-tool--create
	                :name "Boom"
	                :renderer (lambda (_name _args _result _data)
	                            (error "Oops"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (mevedel-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'mevedel (caar warnings)))
      (should (string-match-p "failed" (cadar warnings))))))

(mevedel-deftest mevedel-view--segment-rendering/generic-fallback
  (:before-each (mevedel-tool-clear-registry)
   :after-each (mevedel-tool-clear-registry))
  ,test
  (test)
  :doc "renders registered tools without renderer through generic fallback"
  (progn
    (mevedel-tool-register
     (mevedel-tool--create
      :name "NoRenderer"
      :category "mevedel"
      :args '((path string :required "Path"))))
    (with-temp-buffer
      (insert "(:name \"NoRenderer\" :args (:path \"foo.el\"))\nline\n")
      (let ((rendering (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (equal "NoRenderer: foo.el (1 line)"
                       (plist-get rendering :header)))
        (should (equal "line" (plist-get rendering :body))))))
  :doc "renders unregistered third-party-style tools through generic fallback"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nanswer\n")
    (let ((rendering (mevedel-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty: thing (1 line)"
                     (plist-get rendering :header)))))
  :doc "malformed args in parseable tool calls fall back without signalling"
  (with-temp-buffer
    (insert "(:name \"ThirdParty\" :args \"not a plist\")\nanswer\n")
    (let ((rendering (mevedel-view--segment-rendering
                      (current-buffer) (point-min) (point-max))))
      (should (equal "ThirdParty (1 line)"
                     (plist-get rendering :header)))))
  :doc "renderer opt-out falls through to generic error rendering"
  (progn
    (mevedel-tool-register
     (mevedel-tool--create
      :name "Edit"
      :category "mevedel"
      :display-arg :file_path
      :renderer (lambda (_name _args _result _data) nil)))
    (with-temp-buffer
      (insert "(:name \"Edit\" :args (:file_path \"mevedel-tool-plan.el\"))\n"
              "Error: Could not find old_string in file: x\n")
      (let ((rendering (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max))))
        (should (equal "Edit: mevedel-tool-plan.el (error)"
                       (plist-get rendering :header)))
        (should (eq 'error (plist-get rendering :status)))
        (should (string-prefix-p "Error:" (plist-get rendering :body))))))
  :doc "collapsed cached renderings omit bodies but expansion keeps them"
  (let ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
        (mevedel-view--render-cache-entries 0))
    (with-temp-buffer
      (insert "(:name \"ThirdParty\" :args (:query \"thing\"))\nlarge body\n")
      (let ((collapsed (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t))
            (expanded (mevedel-view--segment-rendering
                       (current-buffer) (point-min) (point-max))))
        (should (equal "ThirdParty: thing (1 line)"
                       (plist-get collapsed :header)))
        (should-not (plist-get collapsed :body))
        (should (equal "large body" (plist-get expanded :body))))))
  :doc "unrelated appends keep completed tool renderings cached"
  (let ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
        (mevedel-view--render-cache-entries 0)
        (calls 0))
    (mevedel-tool-register
     (mevedel-tool--create
      :name "CacheTool"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %s" result)
                        :body result
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let* ((seg-start (point-min))
             (seg-end (point-max))
             (first (mevedel-view--segment-rendering
                     (current-buffer) seg-start seg-end t)))
        (goto-char (point-max))
        (insert "unrelated streaming text\n")
        (let ((second (mevedel-view--segment-rendering
                       (current-buffer) seg-start seg-end t)))
          (should (equal "CacheTool: one" (plist-get first :header)))
          (should (equal "CacheTool: one" (plist-get second :header)))
          (should (= 1 calls))))))
  :doc "tool text changes invalidate cached renderings"
  (let ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
        (mevedel-view--render-cache-entries 0)
        (calls 0))
    (mevedel-tool-register
     (mevedel-tool--create
      :name "CacheTool"
      :category "mevedel"
      :renderer (lambda (_name _args result _data)
                  (cl-incf calls)
                  (list :header (format "CacheTool: %s" result)
                        :body result
                        :initially-collapsed-p t))))
    (with-temp-buffer
      (insert "(:name \"CacheTool\" :args (:query \"thing\"))\none\n")
      (let ((seg-start (point-min))
            (seg-end (point-max)))
        (let ((first (mevedel-view--segment-rendering
                      (current-buffer) seg-start seg-end t)))
          (goto-char (point-min))
          (search-forward "one")
          (replace-match "two" nil t)
          (let ((second (mevedel-view--segment-rendering
                         (current-buffer) seg-start seg-end t)))
            (should (equal "CacheTool: one" (plist-get first :header)))
            (should (equal "CacheTool: two" (plist-get second :header)))
            (should (= 2 calls)))))))
  :doc "session-only blocked state invalidates cached Agent renderings"
  (let* ((mevedel-view--tool-rendering-cache (make-hash-table :test #'equal))
         (mevedel-view--render-cache-entries 0)
         (agent-id "explorer--blocked-cache")
         (workspace (mevedel-workspace--create
                     :type 'project
                     :id "blocked-cache"
                     :root temporary-file-directory
                     :name "blocked-cache"))
         (session (mevedel-session-create "main" workspace))
         (agent-tool (mevedel-tool--create
                      :name "Agent"
                      :renderer #'mevedel-tool-ui--render-agent)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"cache\"))\n"
              "Agent is running.\n"
              (mevedel-pipeline--format-render-data-block
               (list :kind 'agent-transcript
                     :agent-id agent-id
                     :status 'running
                     :calls 1)))
      (cl-letf (((symbol-function 'mevedel-tool-get)
                 (lambda (name &optional _category)
                   (and (equal name "Agent") agent-tool))))
        (let ((running (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t)))
          (should (string-match-p "\\[running · 1 calls\\]"
                                  (plist-get running :header))))
        (setf (mevedel-session-permission-queue session)
              (list (list :origin agent-id)))
        (let ((blocked (mevedel-view--segment-rendering
                        (current-buffer) (point-min) (point-max) t)))
          (should (string-match-p "\\[blocked · awaiting permission\\]"
                                  (plist-get blocked :header)))))))
  :doc "malformed tool text still returns nil"
  (with-temp-buffer
    (insert "not a tool")
    (should-not (mevedel-view--segment-rendering
                 (current-buffer) (point-min) (point-max)))))

(mevedel-deftest mevedel-view--insert-rendered-tool/non-expandable ()
  ,test
  (test)
  :doc "non-expandable renderings do not carry source state"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering '(:header "TaskCreate: Created 1 task"
                       :expandable-p nil))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 10))
      (goto-char (point-min))
      (should (eq 'tool-event
                  (get-text-property (point) 'mevedel-view-type)))
      (should-not (get-text-property (point) 'mevedel-view-source))
      (let ((before (buffer-string)))
        (should-error (mevedel-view-toggle-section))
        (should (equal before (buffer-string)))))))

(mevedel-deftest mevedel-view--insert-rendered-tool/hook-audits ()
  ,test
  (test)
  :doc "renders updated tool input and result audit details"
  (with-temp-buffer
    (mevedel-view-mode)
    (let ((rendering
           '(:header "Read: /tmp/file (1 line)"
                     :body "updated result"
                     :initially-collapsed-p t
                     :hook-audits
                     ((:type tool-input-rewrite
                             :event "PreToolUse"
                             :original-input (:file_path "/tmp/old")
                             :updated-input (:file_path "/tmp/new")
                             :reason "normalized")
                      (:type tool-result-rewrite
                             :event "PostToolUse"
                             :original-result "original result"
                             :updated-result "updated result"
                             :reason "redacted"))))
          (inhibit-read-only t))
      (mevedel-view--insert-rendered-tool rendering (cons 1 40))
      (let ((text (buffer-string)))
        (should (string-match-p "hook changed tool input" text))
        (should (string-match-p "hook changed tool result" text))
        (should-not (string-match-p "original result" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool input")
      (mevedel-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Original input" text))
        (should (string-match-p ":file_path \"/tmp/old\"" text))
        (should (string-match-p "Updated input" text))
        (should (string-match-p ":file_path \"/tmp/new\"" text)))
      (goto-char (point-min))
      (search-forward "hook changed tool result")
      (mevedel-view-toggle-section)
      (let ((text (buffer-string)))
        (should (string-match-p "Original result" text))
        (should (string-match-p "original result" text))
        (should (string-match-p "Updated result" text))
        (should (string-match-p "updated result" text))))))


;;
;;; Tool-call parsing with render-data

(mevedel-deftest mevedel-view--tool-call-parse ()
  ,test
  (test)
  :doc "extracts name, args, and result from a tool segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nplain result\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (equal '(:file_path "/tmp/f") (plist-get call :args)))
        (should (string-match-p "plain result" (plist-get call :result)))
        (should (null (plist-get call :render-data))))))
  :doc "renderer path survives incremental rerender starting inside tool run"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "prefix\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n(:name \"Grep\" :args (:pattern \"task\"))\n\nNo matches found\n"
     '(tool . "call_1"))
    (mevedel-view-test--insert-data data-buf "suffix\n" 'response)
    (with-current-buffer data-buf
      (let* ((tool-start (next-single-property-change (point-min) 'gptel))
             (mid-start (+ tool-start 2))
             (mid-end (+ tool-start 12))
             (segs (mevedel-transcript--extract-segments mid-start mid-end))
             (tool-seg (car segs))
             (call (mevedel-view--tool-call-parse
                    data-buf (cadr tool-seg) (caddr tool-seg))))
        (should (= 1 (length segs)))
        (should (equal "Grep" (plist-get call :name)))
        (should (equal '(:pattern "task") (plist-get call :args)))
        (should (string-match-p "No matches found" (plist-get call :result))))))
  :doc "parser skips leading org drawer and block scaffolding"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat ":PROPERTIES:\n:GPTEL_MODEL: x\n:END:\n"
             "#+begin_tool (Read :file_path \"/tmp/f\")\n"
             "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
             "file body\n"
             "#+end_tool\n")
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (string-match-p "file body" (plist-get call :result)))
        (should-not (string-match-p "GPTEL_MODEL"
                                    (plist-get call :result))))))
  :doc "decodes embedded render-data and strips it from :result"
  (mevedel-view-test--with-buffers
    (let* ((render-data '(:kind diff :patch "--- a\n+++ b\n+hi\n"
                          :path "/tmp/f" :rel-path "f"))
           (body (concat "visible body"
                         (mevedel-pipeline--format-render-data-block
                          render-data))))
      (mevedel-view-test--insert-data
       data-buf
       (concat "(:name \"Edit\" :args (:file_path \"/tmp/f\"))\n\n"
               body "\n")
       '(tool . "call_1"))
      (with-current-buffer data-buf
        (let ((call (mevedel-view--tool-call-parse
                     data-buf (point-min) (point-max))))
	        (should (equal "Edit" (plist-get call :name)))
	        (should (equal "visible body" (plist-get call :result)))
	        (should (equal render-data (plist-get call :render-data)))))))
  :doc "preserves literal trailing end-tool marker in unwrapped result"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n#+end_tool"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (equal "body\n#+end_tool" (plist-get call :result))))))
  :doc "recovers render-data when restored bounds start inside the tool marker"
  (mevedel-view-test--with-buffers
    (let* ((render-data '(:kind diff :patch "--- a\n+++ b\n+hi\n"
                          :path "/tmp/f" :rel-path "f"))
           (block
            (concat "#+begin_tool (RecoverEdit :file_path \"/tmp/f\")\n"
                    "(:name \"RecoverEdit\" :args (:file_path \"/tmp/f\"))\n\n"
                    "visible body"
                    (mevedel-pipeline--format-render-data-block render-data)
                    "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "visible body")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverEdit" (plist-get call :name)))
            (should (equal "visible body" (plist-get call :result)))
            (should (equal render-data (plist-get call :render-data))))
          (mevedel-tool-register
           (mevedel-tool--create
            :name "RecoverEdit"
            :category "mevedel"
            :renderer
            (lambda (_name _args _result data)
              (and (eq (plist-get data :kind) 'diff)
                   (list :header "Recovered diff"
                         :body (plist-get data :patch))))))
          (should (equal "Recovered diff"
                         (plist-get
                          (mevedel-view--segment-rendering
                           data-buf seg-start seg-end)
                          :header)))))))
  :doc "recovers full tool body when result contains marker-looking lines"
  (mevedel-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "before\n"
                "#+end_tool\n"
                         "After\n"
                         "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "After")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (equal "before\n#+end_tool\nAfter"
                           (plist-get call :result))))))))
  :doc "recovers full tool body when stale bounds stop before a literal close"
  (mevedel-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                         "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                         "before\n"
                         "#+end_tool\n"
                         "after\n"
                         "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "before")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (equal "before\n#+end_tool\nafter"
                           (plist-get call :result))))))))
  :doc "keeps persisted-looking tool text inside unwrapped tool results"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
             "outer before\n"
             "#+begin_tool (Bash :command \"echo nested\")\n"
             "(:name \"Bash\" :args (:command \"echo nested\"))\n"
             "nested result\n"
             "#+end_tool\n"
             "outer after\n")
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "Read" (plist-get call :name)))
        (should (string-match-p "outer before" (plist-get call :result)))
        (should (string-match-p
                 "#\\+begin_tool (Bash :command \"echo nested\")"
                 (plist-get call :result)))
        (should (string-match-p "outer after" (plist-get call :result))))))
  :doc "recovers full tool body when result contains persisted-looking tool text"
  (mevedel-view-test--with-buffers
    (let ((block (concat "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                         "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                         "outer before\n"
                         "#+begin_tool (Bash :command \"echo nested\")\n"
                         "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                         "nested result\n"
                         "#+end_tool\n"
                         "outer after\n"
                         "#+end_tool\n")))
      (mevedel-view-test--insert-data data-buf block '(tool . "call_1"))
      (with-current-buffer data-buf
        (let (seg-start seg-end)
          (goto-char (point-min))
          (search-forward ":file_path")
          (setq seg-start (match-beginning 0))
          (search-forward "outer after")
          (setq seg-end (match-end 0))
          (let ((call (mevedel-view--tool-call-parse
                       data-buf seg-start seg-end)))
            (should (equal "RecoverRead" (plist-get call :name)))
            (should (string-match-p
                     "#\\+begin_tool (Bash :command \"echo nested\")"
                     (plist-get call :result)))
            (should (string-match-p
                     "outer after"
                     (plist-get call :result))))))))
  :doc "does not parse a previous quoted tool block as a later tool call"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (real-start real-end)
        (insert "#+begin_tool (Read :file_path \"/quoted\")\n"
                "(:name \"Read\" :args (:file_path \"/quoted\"))\n"
                "quoted body\n"
                "#+end_tool\n\n"
                "Normal assistant text before real tool.\n")
        (put-text-property (save-excursion
                             (goto-char (point-min))
                             (search-forward "Normal")
                             (match-beginning 0))
                           (point)
                           'gptel 'response)
        (setq real-start (point))
        (insert "#+begin_tool (Bash :command \"echo real\")\n"
                "(:name \"Bash\" :args (:command \"echo real\"))\n"
                "real output\n"
                "#+end_tool\n")
        (setq real-end (point))
        (put-text-property (+ real-start 20) (- real-end 12)
                           'gptel '(tool . "call_real"))
        (let ((call (mevedel-view--tool-call-parse
                     data-buf (+ real-start 20) (- real-end 12))))
          (should (equal "Bash" (plist-get call :name)))
          (should (equal "real output" (plist-get call :result)))))))
  :doc "unescapes org-mode tool result storage markers"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "(:name \"WebFetch\" :args (:url \"https://example.com\"))\n\n,* Heading\n,,* Literal comma-star\n,#+begin_src text\nbody\n,#+end_src\n"
     '(tool . "call_1"))
    (with-current-buffer data-buf
      (let ((call (mevedel-view--tool-call-parse
                   data-buf (point-min) (point-max))))
        (should (equal "* Heading\n,* Literal comma-star\n#+begin_src text\nbody\n#+end_src"
                       (plist-get call :result))))))
  :doc "returns nil on unreadable segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf "(:unclosed\n" '(tool . "call_1"))
    (with-current-buffer data-buf
      (should (null (mevedel-view--tool-call-parse
                     data-buf (point-min) (point-max))))))

  :doc "splits compacted malformed tools before the next real tool block"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (second-start)
        (insert "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n")
        (let ((bad-start (point)))
          (insert "(:name \"Edit\" :args (:file_path \"mevedel-chat.el\" :old_string \"unterminated\n")
          (insert "[mevedel: tool output truncated; omitted 8858 chars]\n")
          (put-text-property bad-start (point) 'gptel '(tool . "bad-edit")))
        (setq second-start (point))
        (insert "#+begin_tool (Read :file_path \"next.el\")\n"
                "(:name \"Read\" :args (:file_path \"next.el\"))\n\n"
                "body\n#+end_tool\n")
        (put-text-property second-start (point) 'gptel '(tool . "read"))
        (let ((tool-segs (cl-remove-if-not
                          (lambda (seg) (eq (car seg) 'tool))
                          (mevedel-transcript--extract-segments
                           (point-min) (point-max)))))
          (should (= 2 (length tool-segs)))
          (let ((second-call (mevedel-view--tool-call-parse
                              data-buf (cadr (cadr tool-segs))
                              (caddr (cadr tool-segs)))))
            (should (equal "Read" (plist-get second-call :name)))))))))


(mevedel-deftest mevedel-view--tool-call-parse/malformed-without-marker ()
  ,test
  (test)
  :doc "splits malformed tools before the next real tool block without marker"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (second-start)
        (insert "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n")
        (let ((bad-start (point)))
          (insert "(:name \"Edit\" :args (:file_path \"mevedel-chat.el\" :old_string \"unterminated\n")
          (put-text-property bad-start (point) 'gptel '(tool . "bad-edit")))
        (setq second-start (point))
        (insert "#+begin_tool (Read :file_path \"next.el\")\n"
                "(:name \"Read\" :args (:file_path \"next.el\"))\n\n"
                "body\n#+end_tool\n")
        (put-text-property second-start (point) 'gptel '(tool . "read"))
        (let ((tool-segs (cl-remove-if-not
                          (lambda (seg) (eq (car seg) 'tool))
                          (mevedel-transcript--extract-segments
                           (point-min) (point-max)))))
          (should (= 2 (length tool-segs)))
          (let ((second-call (mevedel-view--tool-call-parse
                              data-buf (cadr (cadr tool-segs))
                              (caddr (cadr tool-segs)))))
            (should (equal "Read" (plist-get second-call :name)))))))))


(mevedel-deftest mevedel-view--tool-fallback-line ()
  ,test
  (test)
  :doc "uses org tool headers for malformed compact fallback summaries"
  (let* ((line (mevedel-view--tool-fallback-line
                "#+begin_tool (Edit :file_path \"mevedel-chat.el\" :old_string \"...\")\n(:name \"Edit\" :args (:file_path \"mevedel-chat.el\" :old_string \"unterminated\n[mevedel: tool output truncated; omitted 8858 chars]\n"))
         (plain (substring-no-properties line)))
    (should (string-match-p "Edit" plain))
    (should (string-match-p "mevedel-chat.el" plain))
    (should-not (string-match-p "#\\+begin_tool" plain))))


(mevedel-deftest mevedel-view--pending-tool-key
  (:doc "keys pending tool calls by call id when available")
  ,test
  (test)
  :doc "uses backend call id before name/args fingerprint"
  (should (equal "call-1"
                 (mevedel-view--pending-tool-key
                  '(:id "call-1" :name "Read" :args (:file_path "a")))))
  :doc "identical calls with distinct ids stay distinct"
  (should-not (equal
               (mevedel-view--pending-tool-key
                '(:id "call-1" :name "Read" :args (:file_path "a")))
               (mevedel-view--pending-tool-key
                '(:id "call-2" :name "Read" :args (:file_path "a"))))))


(mevedel-deftest mevedel-view--pending-tool-calls
  (:doc "tracks and renders the pending-tool live tail")
  ,test
  (test)

  :doc "pre/post hooks add and remove entries by call id"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-2" :name "Grep" :args (:pattern "x"))))
          (with-current-buffer view-buf
            (should (equal '(("call-1" . "Calling Read: a...")
                             ("call-2" . "Calling Grep: x..."))
                           mevedel-view--pending-tool-calls)))
          (should-not
           (mevedel-view--post-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))))
      (with-current-buffer view-buf
        (should (equal '(("call-2" . "Calling Grep: x..."))
                       mevedel-view--pending-tool-calls)))
      (should (= 3 render-count))))

  :doc "tool-boundary renders are debounced and coalesced"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0.02))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-2" :name "Grep" :args (:pattern "x")))))
        (should (= 0 render-count))
        (let ((deadline (+ (float-time) 1.0)))
          (while (and (= render-count 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (should (= 1 render-count)))))

  :doc "immediate pending live tail inserts above status-zone content"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker status-start)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--status-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail recovers above status-zone content when status marker detaches"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail stays above status-zone content after immediate render"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp calling))
                (should (numberp status))
                (should (< calling status))
                (should (= 1 (cl-loop with start = 0
                                      while (string-match "Calling Read: a"
                                                          text start)
                                      count t
                                      do (setq start (match-end 0))))))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail recovers after existing history when status marker detaches"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail stays after existing live assistant text"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
          (insert (propertize "Assistant\n"
                              'mevedel-view-type 'turn-header
                              'mevedel-view-turn-role 'assistant))
          (insert (propertize "Existing response\n"
                              'mevedel-view-type 'response
                              'mevedel-view-source '(1 . 2)))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (existing (string-match-p "Existing response" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp existing))
                (should (numberp calling))
                (should (numberp status))
                (should (< existing calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail stays above propertized status rows"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert (propertize "AGENT STATUS\n"
                              'mevedel-view-type 'agent-handle
                              'mevedel-view-agent-status t))
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "AGENT STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail ignores stale attached status marker"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker (point-min))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (header (string-match-p "mevedel" text))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp header))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< header previous))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail recovers after collapsed turn summaries"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (status-start (marker-position mevedel-view--status-marker)))
          (goto-char status-start)
          (insert (propertize "Previous turn\n"
                              'mevedel-view-type 'turn-summary))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "incremental render preserves status rows when status marker detaches"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render recovers stale in-flight marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render prefers source recovery over stale in-flight marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (mevedel-view--after-header-position)))
        (goto-char (marker-position mevedel-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'mevedel-view-type 'turn-summary))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--in-flight-turn-start history-start)
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view--render-incremental data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (previous (string-match-p "Previous turn" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp previous))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< previous response))
          (should (< response status))))))

  :doc "incremental render ignores stale attached status marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker (point-min))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (header (string-match-p "mevedel" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (calling (string-match-p "Calling Read: a" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp header))
          (should-not old)
          (should (numberp response))
          (should (numberp calling))
          (should (numberp status))
          (should (< header response))
          (should (< response calling))
          (should (< calling status))))))

  :doc "incremental render ignores status marker before existing history"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (history-start (mevedel-view--after-header-position)))
        (goto-char (marker-position mevedel-view--status-marker))
        (insert (propertize "Previous turn\n"
                            'mevedel-view-type 'turn-summary))
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (insert "TASK STATUS\n")
        (set-marker mevedel-view--status-marker history-start)
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view--render-incremental data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (previous (string-match-p "Previous turn" text))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should (numberp previous))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< previous response))
          (should (< response status))))))

  :doc "incremental render ignores status marker inside status row"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (status-start (marker-position mevedel-view--status-marker)))
        (goto-char status-start)
        (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
        (insert (propertize "Assistant\n"
                            'mevedel-view-type 'turn-header
                            'mevedel-view-turn-role 'assistant))
        (insert (propertize "old text\n"
                            'mevedel-view-type 'response
                            'mevedel-view-source '(1 . 2)))
        (let ((row-start (point)))
          (insert (propertize "TASK STATUS\n"
                              'mevedel-view-type 'agent-handle
                              'display "TASK STATUS"
                              'keymap mevedel-view--display-map
                              'read-only t))
          (set-marker mevedel-view--status-marker (+ row-start 5)))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (cl-letf (((symbol-function 'mevedel-view--render-agent-status)
                   (lambda () nil))
                  ((symbol-function 'mevedel-view--interaction-rebuild)
                   (lambda () nil)))
          (should (progn (mevedel-view--render-incremental data-buf) t)))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (old (string-match-p "old text" text))
               (response (string-match-p "assistant text" text))
               (status (string-match-p "TASK STATUS" text)))
          (should-not old)
          (should (numberp response))
          (should (numberp status))
          (should (< response status))))))

  :doc "incremental render restores aggregate status when status marker detaches"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:agent-id "verifier--abc123"
                   :status running
                   :agent-type "verifier"
                   :description "verify"
                   :calls 1)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (mevedel-view--render-agent-status)
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (set-marker mevedel-view--status-marker nil)
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (mevedel-view--render-incremental data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Agent: verifier" text))
                 (prompt (string-match-p "^> " text)))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (numberp prompt))
            (should (< response status))
            (should (< status prompt)))))))

  :doc "aggregate status stays below pending live tail with detached status marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:agent-id "verifier--abc123"
                   :status running
                   :agent-type "verifier"
                   :description "verify"
                   :calls 1)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (set-marker mevedel-view--status-marker nil)
            (set-marker mevedel-view--interaction-marker (point))
            (set-marker mevedel-view--input-marker (point))
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min))))
            (setq mevedel-view--pending-tool-calls
                  (list (cons "call-1" "Calling Read: a"))))
          (should (progn (mevedel-view--render-incremental data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (calling (string-match-p "Calling Read: a" text))
                 (status (string-match-p "Agent: verifier" text)))
            (should-not old)
            (should (numberp response))
            (should (numberp calling))
            (should (numberp status))
            (should (< response calling))
            (should (< calling status)))))))

  :doc "aggregate status ignores stale attached status marker"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((row '(:agent-id "verifier--abc123"
                   :status running
                   :agent-type "verifier"
                   :description "verify"
                   :calls 1)))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () (list row))))
          (let ((inhibit-read-only t))
            (goto-char (marker-position mevedel-view--status-marker))
            (setq mevedel-view--in-flight-turn-start (copy-marker (point)))
            (insert (propertize "Assistant\n"
                                'mevedel-view-type 'turn-header
                                'mevedel-view-turn-role 'assistant))
            (insert (propertize "old text\n"
                                'mevedel-view-type 'response
                                'mevedel-view-source '(1 . 2)))
            (insert "TASK STATUS\n")
            (set-marker mevedel-view--status-marker (point-min))
            (set-marker mevedel-view--interaction-marker (point))
            (set-marker mevedel-view--input-marker (point))
            (setq mevedel-view--data-turn-start
                  (with-current-buffer data-buf
                    (copy-marker (point-min)))))
          (should (progn (mevedel-view--render-incremental data-buf) t))
          (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                 (header (string-match-p "mevedel" text))
                 (old (string-match-p "old text" text))
                 (response (string-match-p "assistant text" text))
                 (status (string-match-p "Agent: verifier" text)))
            (should (numberp header))
            (should-not old)
            (should (numberp response))
            (should (numberp status))
            (should (< header response))
            (should (< response status)))))))

  :doc "pending live tail ignores stale input marker"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (mevedel-view--insert-user-message "Previous turn")
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--input-marker (point-min))
          (setq mevedel-view--in-flight-turn-start (point-min))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (previous (string-match-p "Previous turn" text))
                     (calling (string-match-p "Calling Read: a" text))
                     (status (string-match-p "TASK STATUS" text)))
                (should (numberp previous))
                (should (numberp calling))
                (should (numberp status))
                (should (< previous calling))
                (should (< calling status))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "pending live tail tolerates detached status and input markers"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char (marker-position mevedel-view--status-marker))
          (insert "TASK STATUS\n")
          (set-marker mevedel-view--status-marker nil)
          (set-marker mevedel-view--input-marker nil)
          (setq mevedel-view--in-flight-turn-start (point-min))
          (setq mevedel-view--data-turn-start
                (with-current-buffer data-buf
                  (copy-marker (point-min))))))
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                     (lambda (&rest _) (cl-incf render-count))))
            (with-current-buffer data-buf
              (should-not
               (mevedel-view--pre-tool-hook
                '(:id "call-1" :name "Read" :args (:file_path "a")))))
            (with-current-buffer view-buf
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (calling (string-match-p "Calling Read: a" text))
                     (header (string-match-p "mevedel" text)))
                (should (numberp calling))
                (should (numberp header))
                (should (< header calling))
                (should (= 0 render-count)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "incremental render tolerates detached status and input markers"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "assistant text\n" 'response)
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (set-marker mevedel-view--status-marker nil)
        (set-marker mevedel-view--input-marker nil)
        (setq mevedel-view--in-flight-turn-start (point-min))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min))))
        (setq mevedel-view--pending-tool-calls
              (list (cons "call-1" "Calling Read: a")))
        (should (progn (mevedel-view--render-incremental data-buf) t))
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
               (header (string-match-p "mevedel" text))
               (calling (string-match-p "Calling Read: a" text)))
          (should (numberp header))
          (should (numberp calling))
          (should (< header calling))))))

  :doc "tool hooks do not return rendered agent-status strings to gptel"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
    (let ((mevedel-view-tool-boundary-render-delay 0))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _)
                   #(" ─── agents: 1 running [+] ─────────────────────────────────\n"
                     0 61 (font-lock-face mevedel-view-zone-separator)))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))
          (should-not
           (mevedel-view--post-tool-hook
            '(:id "call-1" :name "Read" :args (:file_path "a"))))))))

  :doc "Agent pre-tool hook does not add a duplicate pending Calling Agent line"
  (mevedel-view-test--with-buffers
    (let ((render-count 0)
          (mevedel-view-tool-boundary-render-delay 0))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-min)))))
      (cl-letf (((symbol-function 'mevedel-view--render-incremental)
                 (lambda (&rest _) (cl-incf render-count))))
        (with-current-buffer data-buf
          (should-not
           (mevedel-view--pre-tool-hook
            '(:id "call-1" :name "Agent"
              :args (:subagent_type "explorer"))))))
      (with-current-buffer view-buf
        (should-not mevedel-view--pending-tool-calls)
        (should (= 1 render-count)))))

  :doc "rendering caps visible calls and adds a truncation tail"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-pending-tools-visible-max 2))
        (setq mevedel-view--pending-tool-calls
              '(("1" . "Calling Read...")
                ("2" . "Calling Grep...")
                ("3" . "Calling Bash...")))
        (mevedel-view--insert-pending-tool-lines
         (cl-subseq mevedel-view--pending-tool-calls 0 2))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Calling Read" text))
          (should (string-match-p "Calling Grep" text))
          (should-not (string-match-p "Calling Bash" text))
          (should (string-match-p "1 more tools running" text))))))

  :doc "post-tool hook removes only the completed pending fragment"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-tool-boundary-render-delay 60))
      (with-current-buffer view-buf
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq mevedel-view--pending-tool-calls
              '(("call-1" . "Calling Read...")
                ("call-2" . "Calling Grep...")))
        (mevedel-view--refresh-pending-tool-lines))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (mevedel-view--post-tool-hook
               '(:id "call-1" :name "Read" :args (:file_path "a"))))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) (point-max))))
                (should-not (string-match-p "Calling Read" text))
                (should (string-match-p "Calling Grep" text))
                (goto-char (point-min))
                (should (search-forward "Calling Grep" nil t))
                (let ((grep-pos (match-beginning 0)))
                  (should (eq 'history-live
                              (get-text-property
                               grep-pos 'mevedel-view-fragment-namespace)))
                  (should (equal "call-2"
                                 (get-text-property
                                  grep-pos 'mevedel-view-fragment-id))))
                (should (equal '(("call-2" . "Calling Grep..."))
                               mevedel-view--pending-tool-calls)))))
        (with-current-buffer view-buf
          (mevedel-view--cancel-tool-boundary-render)))))

  :doc "post-tool hook deletes the live tail when no replacement text is ready"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Agent: explorer...")))
      (mevedel-view--insert-pending-tool-lines
       mevedel-view--pending-tool-calls)
      (should (string-match-p "Calling Agent"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
    (with-current-buffer data-buf
      (mevedel-view--post-tool-hook
       '(:id "call-1" :name "Agent" :args (:subagent_type "explorer"))))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should-not (string-match-p "Calling Agent" text))
        (should-not mevedel-view--pending-tool-calls))))

  :doc "final response render clears pending live tail before rendering"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "final answer\n" 'response)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min))))
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Agent: explorer...")))
      (mevedel-view--insert-pending-tool-lines
       mevedel-view--pending-tool-calls))
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "final answer" text))
        (should-not (string-match-p "Calling Agent" text))
        (should-not mevedel-view--pending-tool-calls))))

  :doc "full rerender preserves ordinary calling text and recreates pending fragments"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Read...")))
      (let ((inhibit-read-only t)
            (tail-start (marker-position mevedel-view--input-marker)))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (insert "Assistant\n| Calling Read...\n")
        (setq mevedel-view--in-flight-turn-start
              (copy-marker tail-start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (= 2 (mevedel-view-test--count-substring
                      "Calling Read" text))))
      (let (plain-seen fragment-pos)
        (goto-char (point-min))
        (while (search-forward "Calling Read" nil t)
          (let ((pos (match-beginning 0)))
            (if (eq 'history-live
                    (get-text-property pos 'mevedel-view-fragment-namespace))
                (setq fragment-pos pos)
              (setq plain-seen t))))
        (should plain-seen)
        (should fragment-pos)
        (should (equal "call-1"
                       (get-text-property
                        fragment-pos 'mevedel-view-fragment-id)))
        (should (eq mevedel-view--pending-tool-region-overlay
                    (get-text-property
                     fragment-pos 'mevedel-view-fragment-region))))))

  :doc "cleanup ignores legacy spinner-frame-only live tails"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t))
        (goto-char mevedel-view--input-marker)
        (insert (propertize "⠸"
                            'mevedel-view-inline-spinner-frame t
                            'font-lock-face 'mevedel-view-ephemeral)
                (propertize " Calling Agent: explorer...\n"
                            'font-lock-face 'mevedel-view-ephemeral)))
      (mevedel-view--delete-pending-tool-live-lines)
      (should (string-match-p "Calling Agent"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))))

  :doc "cleanup ignores stale pending regions without live fragments"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            start)
        (goto-char mevedel-view--input-marker)
        (setq start (point))
        (insert "Calling Read...\n")
        (setq mevedel-view--pending-tool-region-overlay
              (make-overlay start (point) (current-buffer) nil nil)))
      (mevedel-view--delete-pending-tool-live-lines)
      (should (string-match-p "Calling Read"
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
      (should-not mevedel-view--pending-tool-region-overlay)))

  :doc "incremental render preserves live tail when no replacement content is ready"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((inhibit-read-only t)
            (start nil))
        (goto-char mevedel-view--input-marker)
        (set-marker-insertion-type mevedel-view--input-marker t)
        (setq start (point))
        (insert "Assistant\n› Calling Read: mevedel-pipeline.el...\n")
        (setq mevedel-view--in-flight-turn-start (copy-marker start nil))
        (set-marker mevedel-view--status-marker (point))
        (set-marker mevedel-view--interaction-marker (point))
        (set-marker mevedel-view--input-marker (point))
        (set-marker-insertion-type mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (setq mevedel-view--pending-tool-calls nil)
      (mevedel-view--render-incremental data-buf)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Calling Read: mevedel-pipeline.el" text)))))

  :doc "explicit response bounds do not blank live tail without replacement content"
  (mevedel-view-test--with-buffers
    (let (start end)
      (with-current-buffer data-buf
        (setq start (point-max))
        (setq end (point-max)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t)
              (tail-start nil))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (setq tail-start (point))
          (insert "Assistant\n... Thinking... (1 lines)\nCalling Read...\n")
          (setq mevedel-view--in-flight-turn-start
                (copy-marker tail-start nil))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point))
          (set-marker mevedel-view--input-marker (point))
          (set-marker-insertion-type mevedel-view--input-marker nil))
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf (copy-marker (point-max))))
        (setq mevedel-view--pending-tool-calls nil)
        (mevedel-view--render-incremental data-buf start end)
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (string-match-p "Assistant" text))
          (should (string-match-p "Calling Read" text))))))

  :doc "incremental render keeps progress row beside pending tool details"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max))))
      (mevedel-view--start-spinner "Thinking...")
      (setq mevedel-view--pending-tool-calls
            '(("call-1" . "Calling Read...")))
      (mevedel-view--render-incremental data-buf)
      (should (mevedel-view--request-progress-visible-p))
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should (string-match-p "Working" text))
        (should (string-match-p "Calling Read" text))))))


;;
;;; Re-render idempotence with renderer

(mevedel-deftest mevedel-view--renderer-idempotent ()
  ,test
  (test)
  :doc "invoking the renderer twice with identical inputs yields equal plists"
  (let* ((calls 0)
         (tool (mevedel-tool--create
                :name "Idem"
                :renderer (lambda (_name _args _result data)
                            (cl-incf calls)
                            (list :header (format "I:%s" (plist-get data :n))
                                  :body "b"
                                  :body-mode 'diff-mode))))
         (data '(:n 7))
         (args '(:x 1))
         (result "done"))
    (let ((first (mevedel-view--invoke-renderer tool data args result))
          (second (mevedel-view--invoke-renderer tool data args result)))
      (should (equal first second))
      (should (= 2 calls))))
  :doc "round-trip through serialization preserves render-data"
  (let* ((data '(:kind diff :patch "@@ @@\n+a\n" :path "/tmp/x"))
         (serialized (mevedel-pipeline--format-render-data-block data))
         (extract (mevedel-pipeline-extract-render-data
                   (concat "result" serialized))))
    (should (equal data (cdr extract)))))

(mevedel-deftest mevedel-view--agent-transcript-render-data ()
  ,test
  (test)
  :doc "renders hidden review progress handles without exposing user_action"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** /review current changes\n"
                                    nil)
    (mevedel-view-test--insert-data
     data-buf
     (mevedel-pipeline--format-render-data-block
      '(:kind agent-transcript
              :agent-id "reviewer--abc"
              :agent-type "reviewer"
              :name "Review"
              :description "current changes"
              :progress-handle review
              :default-expanded t
              :status running
              :calls 1
              :body ""))
     'ignore)
    (mevedel-view-test--insert-data
     data-buf
     "<user_action>\n  <action>review</action>\n  <results>\n  hidden\n  </results>\n</user_action>\n"
     nil)
    (mevedel-view-test--insert-data data-buf "No issues.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-search "/review current changes" text))
        (should (string-search "Review: current changes" text))
        (should-not (string-search "(1 lines)" text))
        (should (string-search "[running" text))
        (should-not (string-search "… waiting" text))
        (should (string-search "No issues." text))
        (should-not (string-search "<user_action>" text))
        (should-not (string-search "hidden" text))
        (should (= 1 (cl-count-if (lambda (line) (string= line "You"))
                                  (split-string text "\n")))))))

  :doc "renders review prompt when synthetic action shares its user segment"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert "*** /review current changes\n")
        (insert (mevedel-pipeline--format-render-data-block
                 '(:kind agent-transcript
                         :agent-id "reviewer--abc"
                         :agent-type "reviewer"
                         :name "Review"
                         :description "current changes"
                         :progress-handle review
                         :default-expanded t
                         :status running
                         :calls 1
                         :body "")))
        (put-text-property (save-excursion
                             (search-backward "<!-- mevedel-render-data -->" start t))
                           (point)
                           'gptel 'ignore)
        (insert "<user_action>\n"
                "  <action>review</action>\n"
                "  <results>\n"
                "  hidden\n"
                "  </results>\n"
                "</user_action>\n")))
    (mevedel-view-test--insert-data data-buf "No issues.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-search "/review current changes" text))
        (should (string-search "Review: current changes" text))
        (should (string-search "No issues." text))
        (should-not (string-search "<user_action>" text))
        (should-not (string-search "hidden" text))
        (should (= 1 (cl-count-if (lambda (line) (string= line "You"))
                                  (split-string text "\n"))))))))


;;
;;; mevedel-view-send slash-fork integration

(mevedel-deftest mevedel-view-send/local-slash-command ()
  ,test
  (test)
  :doc "local slash command string results are shown"
  (mevedel-view-test--with-buffers
    (let ((mevedel-slash-commands
           (list (cons "fake" (lambda (_args) "fake result"))))
          seen)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq seen (apply #'format fmt args)))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "/fake")
          (mevedel-view-send)
          (should (equal "fake result" seen))
          (should (string-empty-p (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-view-send/dollar-text ()
  ,test
  (test)
  :doc "unknown dollar-prefixed input sends as normal prompt text"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vd" :root "/tmp/vd" :name "vd"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           send-called)
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$PATH is relevant")
          (mevedel-view-send)
          (should (string-empty-p (mevedel-view--input-text)))))
      (should send-called)
      (with-current-buffer data-buf
        (should (string-match-p "\\$PATH is relevant"
                                (buffer-string)))))))

(defmacro mevedel-view-test--with-fork-skill (skill-form &rest body)
  "Wire data-buf with a session containing SKILL-FORM, then run BODY.
Binds `data-buf', `view-buf', and `session' in scope.  The skill is
attached via `mevedel-session-skills' so `mevedel-session-get-skill'
finds it during `$' skill dispatch."
  (declare (indent 1) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((ws (mevedel-workspace--create
                 :type 'test :id "vf" :root "/tmp/vf" :name "vf"
                 :file-cache (mevedel-file-cache--create
                              :table (make-hash-table :test #'equal)
                              :order nil :total-bytes 0)))
            (session (mevedel-session-create "main" ws))
            (skill ,skill-form))
       (setf (mevedel-session-skills session) (list skill))
       (with-current-buffer data-buf
         (setq-local mevedel--session session))
       ,@body)))

(mevedel-deftest mevedel-view-send/skill-fork ()
  ,test
  (test)
  :doc "fork skill blocks input, captures the callback, and inserts the result"
  ;; Drive `mevedel-view-send' for a $myfork dispatch with a fork
  ;; skill installed on the session.  We mock `mevedel-skills-invoke'
  ;; to capture the callback so we can verify (a) the view-side spinner
  ;; and turn marker get armed before invocation returns, (b) the input
  ;; is cleared, (c) the eventual fork result is rendered into the data
  ;; buffer with `gptel response' text properties (the contract that
  ;; `--insert-fork-result' relies on for downstream rendering).
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args save-called status-called)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (skill args callback &rest kwargs)
                   (setq captured-args
                         (list :skill skill :args args
                               :callback callback :kwargs kwargs))))
                ((symbol-function 'mevedel-session-persistence-save)
                 (lambda (s b)
                   (setq save-called (list s b))
                   "saved"))
                ((symbol-function 'gptel--update-status)
                 (lambda (&rest args) (setq status-called args))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork run a thing")
          (mevedel-view-send)

          ;; The view armed the in-flight turn marker and spinner.
          (should (markerp mevedel-view--in-flight-turn-start))
          (should (marker-position mevedel-view--in-flight-turn-start))
          (should (mevedel-view--request-progress-visible-p))

          ;; The user-message display text appeared in the view above
          ;; the input region.
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myfork run a thing" text))))
        (with-current-buffer data-buf
          (should mevedel--current-request))

        ;; mevedel-skills-invoke was called with the right shape.
        (should captured-args)
        (should (equal "myfork"
                       (mevedel-skill-name (plist-get captured-args :skill))))
        (should (equal "run a thing" (plist-get captured-args :args)))
        (should (eq 'user-skill
                    (plist-get (plist-get captured-args :kwargs) :trigger)))

        ;; Fire the callback with a fork outcome; expect the data buffer
        ;; to grow an assistant response carrying `gptel response'.
        (with-current-buffer data-buf
          (let ((before (buffer-size)))
            (funcall (plist-get captured-args :callback)
                     '(:status ok :kind fork
                               :result "FORK-RESULT-BODY"
                               :agent-id "myfork--1"))
            (should (> (buffer-size) before))
            (let ((text (buffer-string)))
              (should (string-match-p "FORK-RESULT-BODY" text)))
            ;; The inserted region carries `gptel response' so the view
            ;; renderer treats it as an assistant turn.
            (goto-char (point-max))
            (let ((response-pos
                   (text-property-any (point-min) (point-max)
                                      'gptel 'response)))
              (should response-pos)
              (should (string-match-p
                       "FORK-RESULT-BODY"
                       (buffer-substring-no-properties
                        response-pos (point-max)))))))
        (with-current-buffer data-buf
          (should-not mevedel--current-request)
          (should (= 1 (mevedel-session-turn-count session)))
          (should (equal (list session data-buf) save-called))
          (should (equal '(" Ready" success) status-called)))))

  :doc "fork result prompt rewrite audit rerenders on the submitted user turn"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (_skill _args callback &rest _)
                   (setq captured-args (list :callback callback))))
                ((symbol-function 'mevedel-session-persistence-save)
                 (lambda (&rest _) "saved"))
                ((symbol-function 'gptel--update-status)
                 (lambda (&rest _))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork run")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (funcall (plist-get captured-args :callback)
                   '(:status ok :kind fork
                             :result "agent body"
                             :agent-id "myfork--audit"
                             :hook-audits
                             ((:type prompt-rewrite
                                     :event "UserPromptExpansion"
                                     :original "Original body"
                                     :submitted "Expanded body"
                                     :reason "expanded"))
                             :render-data
                             (:kind agent-transcript
                                    :agent-id "myfork--audit"
                                    :agent-type "general-purpose"
                                    :description "run"
                                    :status completed
                                    :body "agent body"))))
        (with-current-buffer data-buf
          (let* ((text (buffer-string))
                 (audit-pos (string-search
                             "<!-- mevedel-hook-audit -->" text))
                 (response-pos (and audit-pos
                                    (string-search gptel-response-separator
                                                   text (1+ audit-pos))))
                 (render-data (cdr (mevedel-pipeline-extract-render-data
                                    text))))
            (should (string-search "<!-- mevedel-render-data -->" text))
            (should audit-pos)
            (should response-pos)
            (should (< audit-pos response-pos))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-hook-audit -->")
            (should (eq (get-text-property (match-beginning 0) 'gptel)
                        'ignore))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-render-data -->")
            (should (eq (get-text-property (match-beginning 0) 'gptel)
                        'ignore))
            (should-not (plist-get render-data :hook-audits))))
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (let* ((text (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))
                 (hook-pos (string-search "hook changed prompt" text))
                 (agent-pos (string-search
                             "Agent: general-purpose -- run" text)))
            (should (string-match-p "Agent: general-purpose -- run" text))
            (should (string-match-p "hook changed prompt" text))
            (should (< hook-pos agent-pos))
            (should-not (string-match-p "Original body" text)))
          (goto-char (point-min))
          (search-forward "hook changed prompt")
          (mevedel-view-toggle-section)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "UserPromptExpansion" text))
            (should (string-match-p "Original body" text))
            (should (string-match-p "Expanded body" text)))))))

  :doc "fork error stops the spinner and does not insert a response"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (captured-args)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (_skill _args callback &rest _)
                   (setq captured-args (list :callback callback))))
                ((symbol-function 'message)
                 (lambda (&rest _))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork run")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (should mevedel--current-request))
        ;; Drive the error branch.
        (with-current-buffer data-buf
          (let ((before (buffer-string)))
            (funcall (plist-get captured-args :callback)
                     '(:status error :reason boom :message "boom"))
            ;; No response inserted.
            (should (equal before (buffer-string)))
            (should-not mevedel--current-request)))
        ;; Request progress was removed by `--stop-spinner'.
        (with-current-buffer view-buf
          (should-not (mevedel-view--request-progress-visible-p)))))))
)

(mevedel-deftest mevedel-view-send/skill-inline ()
  ,test
  (test)
  :doc "inline skill forwards expanded body with render-data side channel"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let (send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should-not (string-match-p "Expanded hello" text))))
        (with-current-buffer view-buf
          (goto-char (point-min))
          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p "Expanded hello" expanded))
            (should-not (string-match-p "mevedel-render-data" expanded)))
          (mevedel-view-toggle-section))
        (should send-called)
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "Expanded hello" text))
            (should (string-search "<!-- mevedel-render-data -->" text))
            (goto-char (point-min))
            (search-forward "<!-- mevedel-render-data -->")
            (should (eq 'ignore
                        (get-text-property (match-beginning 0)
                                           'gptel)))
            (should (equal
                     (mevedel-pipeline--strip-render-data-blocks text)
                     "\n\n*** Expanded hello\n"))))
	        (with-current-buffer view-buf
	          (mevedel-view--full-rerender)
	          (let ((text (buffer-substring-no-properties
	                       (point-min) mevedel-view--input-marker)))
	            (should (string-match-p "\\$myskill hello" text))
	            (should (string-match-p "Prompt" text))
	            (should-not (string-match-p "Expanded hello" text)))
	          (goto-char (point-min))
	          (search-forward "Prompt")
	          (mevedel-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) mevedel-view--input-marker)))
	            (should (string-match-p "Expanded hello" expanded)))))))

  :doc "inline skill expansion rewrites render hook audit and context"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptExpansion
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook-with-context)))))))
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send)
          (should send-called)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should (string-match-p "hook changed prompt" text))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "model-only context" text)))
          (goto-char (point-min))
          (search-forward "hook changed prompt")
          (mevedel-view-toggle-section)
	          (let ((expanded (buffer-substring-no-properties
	                           (point-min) mevedel-view--input-marker)))
	            (should (string-match-p "UserPromptExpansion" expanded))
	            (should (string-match-p "Expanded hello" expanded))
	            (should (string-match-p "rewritten prompt" expanded)))
	          (goto-char (point-min))
	          (search-forward "hook changed prompt")
	          (mevedel-view-toggle-section)
	          (goto-char (point-min))
	          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "model-only context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "rewritten prompt" text))
          (should (string-match-p "model-only context" text))
          (should (string-match-p "<!-- mevedel-hook-audit -->" text))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
	          (should (string-match-p "hook changed prompt" text))
	          (should (string-match-p "hook context added" text))
	          (should-not (string-match-p "model-only context" text))))))

  :doc "inline attachment failure rolls back echoed prompt"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded"
       :context 'inline
       :user-invocable-p t)
    (let ((path (make-temp-file "mevedel-inline-rollback-"))
          send-called
          message-text)
      (unwind-protect
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest args)
                       (apply #'mevedel-skills--gptel-send-advice
                              (lambda (&rest _)
                                (setq send-called t))
                              args)))
                    ((symbol-function 'mevedel-skills-invoke)
                     (lambda (_skill _args callback &rest _)
                       (funcall callback
                                '(:status error
                                  :reason blocked
                                  :message "blocked"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (mevedel-session-add-dropped-file-grant session path)
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert (format "Please use $myskill @file:%s" path))
              (mevedel-view-send)
              (should-not send-called)
              (should (equal (format "Please use $myskill @file:%s" path)
                             (mevedel-view--input-text)))
              (should-not (mevedel-view-history--entries))
              (should-not (mevedel-view--request-progress-visible-p))
              (let ((history (buffer-substring-no-properties
                              (point-min) mevedel-view--input-marker)))
                (should-not (string-match-p "Please use \\$myskill"
                                            history))))
            (with-current-buffer data-buf
              (should-not (string-match-p "Please use \\$myskill"
                                          (buffer-string))))
            (should (equal (list (expand-file-name path))
                           (mevedel-session-dropped-file-grants session)))
            (should-not (mevedel-session-active-dropped-file-grants session))
            (should (string-match-p "Inline skill failed: blocked"
                                    message-text)))
        (delete-file path)))))

(mevedel-deftest mevedel-view--forward-input-now ()
  ,test
  (test)
  :doc "clears inherited response metadata from forwarded user prompts"
  (mevedel-view-test--with-buffers
    (let (sent)
      (with-current-buffer data-buf
        (insert (propertize "Assistant answer.\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&optional _arg) (setq sent t))))
        (with-current-buffer view-buf
          (mevedel-view--forward-input-now "On a scale?")))
      (should sent)
      (with-current-buffer data-buf
        (goto-char (point-min))
        (search-forward "On a scale?")
        (let ((pos (line-beginning-position))
              (end (line-end-position)))
          (while (< pos end)
            (should-not (get-text-property pos 'gptel))
            (setq pos (1+ pos)))))))

  :doc "attaches pending SessionStart hook context to the submitted prompt"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vf-session-hook" :root "/tmp/vf"
                :name "vf"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (mevedel-hooks-record-session-context
         session
         '(:additional-context ("PONYTAIL MODE ACTIVE - level: full"))
         'SessionStart))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (goto-char (mevedel-view--input-start))
        (insert "Hello"))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&optional _arg) (setq sent t)))
                ((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra)))
        (with-current-buffer view-buf
          (mevedel-view-send)
          (should sent)
          (should-not (mevedel-session-hook-context-pending session))
          (let ((text (buffer-string)))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "PONYTAIL MODE ACTIVE" text)))
          (search-backward "hook context added")
          (mevedel-view-toggle-section)
          (should (string-match-p "SessionStart" (buffer-string)))
          (should (string-match-p "PONYTAIL MODE ACTIVE - level: full"
                                  (buffer-string)))))
      (with-current-buffer data-buf
        (should (string-match-p "<hook-event name=\"SessionStart\">"
                                (buffer-string)))
        (should (string-match-p "PONYTAIL MODE ACTIVE - level: full"
                                (buffer-string)))))))

(mevedel-deftest mevedel-view-send/queued-user-messages ()
  ,test
  (test)

  :doc "plain input during an active request queues instead of sending"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           send-called)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "follow up")
          (mevedel-view-send)
          (should-not send-called)
          (should (string-empty-p (mevedel-view--input-text)))
          (should (equal '("follow up")
                         (mevedel-view-history--entries)))
          (should (equal "1 queued message pending"
                         (mevedel-view--interaction-count-label)))
          (should (string-match-p "follow up"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))
      (should (equal "follow up"
                     (plist-get
                      (car (mevedel-session-queued-user-messages session))
                      :input)))
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))

  :doc "queued inline skill waits for request-time transforms"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "alpha"
       :body "Alpha body"
       :context 'inline
       :user-invocable-p t)
    (let* ((data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           position
           fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session))
        (setq position (copy-marker (point-max) nil)))
      (cl-letf (((symbol-function
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 (lambda () nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "please use $alpha")
          (mevedel-view-send)))
      (let ((entry (car (mevedel-session-queued-user-messages session))))
        (should entry)
        (should-not (plist-get entry :requires-request-transform))
        (should (mevedel-view--queued-user-message-requires-transform-p
                 entry session)))
      (setq fsm
            (gptel-make-fsm
             :info (list :buffer data-buf
                         :backend nil
                         :data data
                         :position position)))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (should (= 1 (length (plist-get data :messages))))
      (with-current-buffer data-buf
        (should-not (string-match-p "queued-user-message"
                                    (buffer-string))))))

  :doc "queued message stays visible across incremental in-flight rendering"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-incremental" :root "/tmp/vq"
                :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           data-turn-start)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session))
        (insert "*** Prompt\n")
        (setq data-turn-start (copy-marker (point-max) nil))
        (let ((start (point)))
          (insert "First partial response.\n")
          (put-text-property start (point) 'gptel 'response)))
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "Prompt"))
        (mevedel-view--render-incremental data-buf)
        (goto-char (mevedel-view--input-start))
        (insert "follow up")
        (mevedel-view-send)
        (should (string-match-p "follow up"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))
        (with-current-buffer data-buf
          (let ((start (point)))
            (insert "Second partial response.\n")
            (put-text-property start (point) 'gptel 'response)))
        (mevedel-view--render-incremental data-buf)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (queued (string-match-p "follow up" text))
               (partial (string-match-p "Second partial response" text))
               (prompt (string-match-p "\n> " text)))
          (should queued)
          (should partial)
          (should prompt)
          (should (< partial queued))
          (should (< queued prompt))))))

  :doc "queued message stays visible across in-flight full rerender"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-full-rerender" :root "/tmp/vq"
                :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session))
        (insert "*** Prompt\n")
        (let ((start (point)))
          (insert "Partial response.\n")
          (put-text-property start (point) 'gptel 'response)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "follow up"
                        :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--data-turn-start
              (with-current-buffer data-buf
                (copy-marker (point-min) nil)))
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (queued (string-match-p "follow up" text))
               (partial (string-match-p "Partial response" text))
               (prompt (string-match-p "\n> " text)))
          (should queued)
          (should partial)
          (should prompt)
          (should (< partial queued))
          (should (< queued prompt))))))

  :doc "queued message UI shows edit-batch and clear key hints"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hint" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "follow up" :display-text "follow up")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--interaction-rebuild)
        (should (string-match-p "C-c C-e edit batch; C-c C-q clear"
                                (buffer-string))))))

  :doc "queue-time UserPromptSubmit stores prepared model input"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           seen-prompt)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event event-plist callback &rest _)
                   (setq seen-prompt (plist-get event-plist :prompt))
                   (funcall callback '(:updated-input "rewritten"))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _)
                   "<hook-context>\n<hook-event name=\"UserPromptSubmit\">\nctx\n</hook-event>\n</hook-context>")))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send)
          (should (string-empty-p (mevedel-view--input-text)))))
      (should (equal "draft" seen-prompt))
      (let ((entry (car (mevedel-session-queued-user-messages session))))
        (should (equal "draft" (plist-get entry :input)))
        (should (equal "draft" (plist-get entry :history-input)))
        (should (equal "rewritten" (plist-get entry :display-text)))
        (should (equal "rewritten\n\n<hook-context>\n<hook-event name=\"UserPromptSubmit\">\nctx\n</hook-event>\n</hook-context>"
                       (plist-get entry :model-input))))))

  :doc "queue-time UserPromptSubmit rewrite audit survives drain and rerender"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-audit" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback '(:updated-input "rewritten"
                                       :system-message "queued rewrite"))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (_event _session _workspace &rest extra) extra))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send))
        (should (= 1 (length (mevedel-session-queued-user-messages session))))
        (should (plist-get (car (mevedel-session-queued-user-messages session))
                           :hook-audits))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should sent))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "<!-- mevedel-hook-audit -->" text))
          (goto-char (point-min))
          (search-forward "<!-- mevedel-hook-audit -->")
          (should (eq 'ignore
                      (get-text-property (match-beginning 0)
                                         'gptel)))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Queued message" text))
          (should (string-match-p "hook changed prompt" text))
          (should-not (string-match-p "queued rewrite" text)))
        (goto-char (point-min))
        (search-forward "hook changed prompt")
        (mevedel-view-toggle-section)
        (let ((expanded (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
          (should (string-match-p "queued rewrite" expanded))
          (should (string-match-p "draft" expanded))
          (should (string-match-p "rewritten" expanded))))))

  :doc "queue-time UserPromptSubmit block leaves composer and queue untouched"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-block" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback '(:continue nil
                                       :stop-reason "blocked"))))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send)
          (should (string= "draft" (mevedel-view--input-text)))))
      (should-not (mevedel-session-queued-user-messages session))
      (should-not (mevedel-view-history--entries))))

  :doc "queue-time async hook does not clear a changed composer"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-async" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           hook-callback)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq hook-callback callback)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil)))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send)
          (should mevedel-view--prompt-hook-pending)
          (goto-char (point-max))
          (insert " changed")
          (funcall hook-callback nil)
          (should-not mevedel-view--prompt-hook-pending)
          (should (string= "draft changed"
                           (mevedel-view--input-text)))))
      (should (= 1 (length (mevedel-session-queued-user-messages
                            session))))))

  :doc "queue-time async hook schedules fallback if request already ended"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-late" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           hook-callback
           drain-buffer
           send-called)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq hook-callback callback)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil))
                ((symbol-function
                  'mevedel-view--schedule-late-queued-user-message-drain)
                 (lambda ()
                   (setq drain-buffer mevedel--data-buffer)
                   (mevedel-view--run-queued-user-message-drain
                    drain-buffer)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (mevedel-view-send))
        (with-current-buffer data-buf
          (setq-local mevedel--current-request nil))
        (with-current-buffer view-buf
          (funcall hook-callback nil)))
      (should (eq drain-buffer data-buf))
      (should send-called)
      (should-not (mevedel-session-queued-user-messages session))
      (with-current-buffer data-buf
        (should (string-match-p "<queued-user-message-batch count=\"1\">"
                                (buffer-string))))))

  :doc "fallback drain preserves queued entries while plan approval is pending"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-fallback-plan" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (with-current-buffer view-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-plan-queue session)
            (list (list :body "# Plan" :origin "main")))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "new feedback"
                        :model-input "new feedback prepared")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf))
      (should-not sent)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (with-current-buffer data-buf
        (should-not (string-match-p "queued-user-message-batch"
                                    (buffer-string))))))

  :doc "fallback drain keeps queued entry when inline fork skill blocks"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "forker"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let (sent)
      (with-current-buffer data-buf
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "please use $forker"
                        :display-text "please use $forker"
                        :model-input "please use $forker")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf))
      (should-not sent)
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (with-current-buffer view-buf
        (should (string-empty-p (mevedel-view--input-text))))
      (with-current-buffer data-buf
        (should-not (string-match-p "queued-user-message-batch"
                                    (buffer-string))))))

  :doc "late drain scheduler uses data buffer after request cleanup"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-hook-late-schedule"
                :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           drain-buffer)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_secs _repeat _function &rest args)
                   (setq drain-buffer (car args))
                   'timer)))
        (with-current-buffer view-buf
          (mevedel-view--schedule-late-queued-user-message-drain)))
      (should (eq drain-buffer data-buf))))

  :doc "interaction rebuild preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-point" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "already queued"
                        :display-text "already queued")))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (goto-char (mevedel-view--input-start))
        (insert "draft")
        (goto-char (+ (mevedel-view--input-start) 2))
        (mevedel-view--interaction-rebuild)
        (should (string= "draft" (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 2))))))

  :doc "spinner status redraw preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (mevedel-view--start-spinner "Thinking...")
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (mevedel-view--update-spinner "Calling Read...")
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (mevedel-view--stop-spinner)
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should (string= draft (mevedel-view--input-text))))))

  :doc "streaming redraw preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (copy-marker mevedel-view--input-marker nil))
        (goto-char (mevedel-view--input-start))
        (insert "draft")
        (goto-char (+ (mevedel-view--input-start) 2))
        (mevedel-view--render-incremental data-buf)
        (should (string= "draft" (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 2))))))

  :doc "streaming redraw preserves composer point in every live window"
  (mevedel-view-test--with-buffers
    (let (data-turn-start)
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max) nil)))
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (switch-to-buffer view-buf)
      (delete-other-windows)
      (let ((first-window (selected-window))
            (second-window (split-window-right)))
        (set-window-buffer second-window view-buf)
        (with-current-buffer view-buf
          (setq mevedel-view--data-turn-start data-turn-start)
          (setq mevedel-view--in-flight-turn-start
                (copy-marker mevedel-view--input-marker nil)))
        (with-selected-window first-window
          (goto-char (mevedel-view--input-start))
          (insert "draft")
          (goto-char (+ (mevedel-view--input-start) 2)))
        (with-selected-window second-window
          (goto-char (+ (mevedel-view--input-start) 3)))
        (with-selected-window first-window
          (mevedel-view--render-incremental data-buf)
          (should (= (window-point first-window)
                     (+ (mevedel-view--input-start) 2)))
          (should (= (window-point second-window)
                     (+ (mevedel-view--input-start) 3)))
          (should (string= "draft" (mevedel-view--input-text))))
        (delete-other-windows))))

  :doc "full rerender preserves composer point while drafting"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (goto-char (+ (mevedel-view--input-start) 2))
      (mevedel-view--full-rerender)
      (should (string= "draft" (mevedel-view--input-text)))
      (should (= (point) (+ (mevedel-view--input-start) 2)))))

  :doc "full rerender preserves multiline composer text starting with >"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (mevedel-view--full-rerender)
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only)))))

  :doc "full rerender preserves composer when all zone markers drift"
  (mevedel-view-test--with-buffers
    (let ((draft "> quoted\nsecond line"))
      (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
      (mevedel-view-test--insert-data data-buf "Assistant text.\n" 'response)
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (mevedel-view--full-rerender)
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4))))))

  :doc "slash input during an active request is rejected"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-slash" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (with-current-buffer view-buf
        (goto-char (mevedel-view--input-start))
        (insert "/review")
        (should-error (mevedel-view-send) :type 'user-error)
        (should-not (mevedel-session-queued-user-messages session))
        (should (string= "/review" (mevedel-view--input-text))))))

  :doc "fallback drain submits queued messages as one FIFO batch"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-fifo" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (sent 0))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (cl-incf sent))))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should (= 1 sent))
        (should-not (mevedel-session-queued-user-messages session))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p
                     "<queued-user-message-batch count=\"2\">" text))
            (should (< (string-match-p "first" text)
                       (string-match-p "second" text))))))))

  :doc "queued messages do not drain while the request is still active"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-active" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (sent nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "pending" :display-text "pending")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should-not sent)
        (should (mevedel-session-queued-user-messages session)))))

  :doc "editing queued messages restores the whole uncommitted batch"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-edit" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (sent nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (with-current-buffer view-buf
        (mevedel-view--interaction-rebuild)
        (mevedel-view-edit-last-queued-message)
        (should (string= "first\n\nsecond" (mevedel-view--input-text)))
        (should-not (mevedel-session-queued-user-messages session)))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-queued-user-message-batch data-buf)
        (should-not sent))))

  :doc "resubmitting an edited queued batch creates one queued entry"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-edit-resubmit" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--current-request
                    (mevedel-request--create :session session)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil))
	                ((symbol-function 'gptel-send)
	                 (lambda (&rest _) (error "Gptel-send should not run"))))
        (with-current-buffer view-buf
          (mevedel-view--interaction-rebuild)
          (mevedel-view-edit-last-queued-message)
          (mevedel-view-send)))
      (let ((queue (mevedel-session-queued-user-messages session)))
        (should (= 1 (length queue)))
        (should (equal "first\n\nsecond"
                       (plist-get (car queue) :input)))
        (should (equal "first\n\nsecond"
                       (plist-get (car queue) :model-input))))))

  :doc "WAIT drain injects all prepared queued entries without gptel-send"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-drain" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf
                             :backend nil
                             :data data)))
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :model-input "first prepared")
                  (list :input "audited"
                        :model-input "audited prepared"
                        :hook-audits
                        '((:type prompt-rewrite
                                  :event "UserPromptSubmit"
                                  :original "secret draft"
                                  :submitted "audited prepared"
                                  :reason "queued rewrite")))
                  (list :input "second" :model-input "second prepared")))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--handle-queued-user-message-inject fsm)
        (should-not sent)
        (should-not (mevedel-session-queued-user-messages session))
        (let ((msgs (plist-get data :messages)))
          (should (= 2 (length msgs)))
          (let ((content (plist-get (aref msgs 1) :content)))
            (should (string-match-p "first prepared" content))
            (should (string-match-p "second prepared" content))
            (should (string-match-p "audited prepared" content))
            (should-not (string-match-p "<!-- mevedel-hook-audit -->"
                                        content))
            (should-not (string-match-p "secret draft" content))
            (should-not (string-match-p "queued rewrite" content))
            (should (< (string-match-p "first prepared" content)
                       (string-match-p "second prepared" content)))))
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "first prepared" text))
            (should (string-match-p "<!-- mevedel-hook-audit -->" text))
            (should-not (string-match-p "secret draft" text))
            (let ((audit (car (mevedel-view--hook-audit-records-from-text
                               text))))
              (should (equal "secret draft"
                             (plist-get audit :original))))))
        (with-current-buffer view-buf
          (mevedel-view--full-rerender)
          (should (string-match-p "second prepared"
                                  (buffer-string))))))))

  :doc "WAIT drain preserves queued entries in Plan mode"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-plan" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active plan turn"))))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf
                             :backend nil
                             :data data))))
      (setf (mevedel-session-permission-mode session) 'plan)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "new feedback"
                        :model-input "new feedback prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (should (= 1 (length (plist-get data :messages))))
      (should (= 1 (length (mevedel-session-queued-user-messages session))))
      (with-current-buffer data-buf
        (should-not (string-match-p "new feedback prepared"
                                    (buffer-string))))))

  :doc "WAIT drain advances response marker after queued batch insertion"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-marker" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           (position nil)
           (data-turn-start nil)
           (fsm nil))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (insert "*** active turn\n")
        (setq data-turn-start (copy-marker (point) nil))
        (let ((start (point)))
          (insert "assistant partial\n")
          (put-text-property start (point) 'gptel 'response))
        (setq position (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "active turn"))
        (mevedel-view--render-incremental data-buf))
      (setq fsm
            (gptel-make-fsm
             :info (list :buffer data-buf
                         :backend nil
                         :data data
                         :position position)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "queued"
                        :model-input "queued prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (with-current-buffer view-buf
        (should (string-match-p "queued prepared"
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))
      (with-current-buffer data-buf
        (goto-char position)
        (insert (propertize "assistant response\n" 'gptel 'response))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
          (should (< (string-match-p "<queued-user-message-batch" text)
                     (string-match-p "assistant response" text)))
          (should (string-match-p "queued prepared" text))))))

  :doc "WAIT drain renders queued batch before first assistant text"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-wait-first" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "active turn"))))
           position
           data-turn-start
           fsm)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (insert "*** active turn\n")
        (setq data-turn-start (copy-marker (point) nil))
        (setq position (copy-marker (point) nil)))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (setq mevedel-view--data-turn-start data-turn-start)
        (setq mevedel-view--in-flight-turn-start
              (mevedel-view--insert-user-message "active turn"))
        (mevedel-view--start-spinner "Working..."))
      (setq fsm
            (gptel-make-fsm
             :info (list :buffer data-buf
                         :backend nil
                         :data data
                         :position position)))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "queued"
                        :model-input "queued prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (with-current-buffer view-buf
        (let* ((text (buffer-substring-no-properties
                      (point-min) (point-max)))
               (label (string-match-p "Queued message" text))
               (queued (string-match-p "queued prepared" text))
               (prompt (string-match-p "\n> " text)))
          (should label)
          (should queued)
          (should prompt)
          (should (< label queued))
          (should (< queued prompt))
          (should-not (string-match-p "<system-reminder>" text))
          (should-not (string-match-p "queued-user-message" text))))))

  :doc "WAIT drain ignores agent FSMs that share the parent session"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-agent-wait" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           (inv (mevedel-agent-invocation--create :background-p t))
           (data (list :messages
                       (vector (list :role "user"
                                     :content "agent turn"))))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf
                             :backend nil
                             :data data
                             :mevedel-agent-invocation inv))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws)
        (setq-local mevedel--agent-invocation inv))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "main follow-up"
                        :model-input "main follow-up prepared")))
      (mevedel-view--handle-queued-user-message-inject fsm)
      (should (mevedel-session-queued-user-messages session))
      (should (= 1 (length (plist-get data :messages))))
      (with-current-buffer data-buf
        (should-not (string-match-p "main follow-up prepared"
                                    (buffer-string))))))

(mevedel-deftest mevedel-view-send/user-prompt-hooks ()
  ,test
  (test)

  :doc "blocking UserPromptSubmit does not fork, record history, or insert prompt"
  (let* ((root (make-temp-file "mevedel-view-hooks" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks" root "view-hooks"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       mevedel-view-test--stop-prompt-hook)))))))
         fork-called)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-session--fork-pending t))
          (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                     (lambda (&rest _)
                       (setq fork-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "blocked prompt")
              (mevedel-view-send)
              (should-not fork-called)
              (should-not (mevedel-view-history--entries))
              (should-not
               (string-match-p
                "blocked prompt"
                (buffer-substring-no-properties
                 (point-min) mevedel-view--input-marker))))
	      (with-current-buffer data-buf
		(should (string-empty-p (buffer-string))))))
      (delete-directory root t)))

  :doc "/plan prompts run UserPromptSubmit and materialize rewind forks"
  (let* ((root (make-temp-file "mevedel-view-plan-hooks" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-plan-hooks" root "view-plan-hooks"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
	                        :hooks ((:type elisp
	                                       :function
	                                       mevedel-view-test--rewrite-prompt-hook-with-context)))))))
         (mevedel-view-test--seen-prompt nil)
         events)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setq-local mevedel-session--fork-pending t))
          (with-current-buffer view-buf
            (setq-local mevedel--session session))
          (cl-letf (((symbol-function 'mevedel-session-persistence-fork-now)
                     (lambda (&rest _)
                       (push 'fork events)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (push 'send events))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "/plan draft")
              (mevedel-view-send)
              (should (equal "draft" mevedel-view-test--seen-prompt))
              (should (equal '(fork send) (nreverse events)))
              (should (string-empty-p (mevedel-view--input-text))))
            (with-current-buffer data-buf
              (let ((text (buffer-string)))
                (should (string-match-p "rewritten prompt" text))
                (should (string-match-p "model-only context" text))
                (should-not (string-match-p "/plan draft" text)))))
          (with-current-buffer view-buf
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should (string-match-p "rewritten prompt" text))
              (should (string-match-p "hook context added" text))
              (should-not (string-match-p "model-only context" text)))))
      (delete-directory root t)))

  :doc "blocking UserPromptSubmit prevents expanded inline skill send"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
	                         :hooks ((:type elisp
	                                        :function
	                                        mevedel-view-test--stop-prompt-hook)))))))
	          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
	(with-current-buffer view-buf
	  (goto-char (mevedel-view--input-start))
	  (insert "$myskill blocked")
	  (mevedel-view-send)
	          (should-not send-called)
	          (should-not (mevedel-view-history--entries)))
	        (with-current-buffer data-buf
	          (should (string-empty-p (buffer-string)))
	          (should-not (bound-and-true-p
	                       mevedel-skills--pending-request-context))))))

  :doc "inline skill hooks see expanded body and can rewrite it"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook)))))))
          (mevedel-view-test--seen-prompt nil)
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send))
	        (should send-called)
	        (should (string-match-p "Expanded hello"
	                                mevedel-view-test--seen-prompt))
	        (should-not (string-search "<!-- mevedel-render-data -->"
	                                   mevedel-view-test--seen-prompt))
	        (with-current-buffer data-buf
	          (let ((text (mevedel--strip-hook-audit-blocks
                         (buffer-string))))
	            (should (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "Expanded hello" text)))))))

  :doc "prompt rewrites render an expandable hook audit disclosure"
  (let* ((root (make-temp-file "mevedel-view-hooks-audit" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-audit" root "view-hooks-audit"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type elisp
                                       :function
                                       mevedel-view-test--rewrite-prompt-hook-with-message)))))))
         (send-called nil))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "original prompt")
              (mevedel-view-send)
              (should send-called)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-match-p "rewritten prompt" text))
                (should (string-match-p "hook changed prompt" text))
                (should-not (string-match-p "original prompt" text))
                (should-not (string-match-p "changed by test hook" text)))
              (goto-char (point-min))
              (search-forward "hook changed prompt")
              (mevedel-view-toggle-section)
              (let ((expanded (buffer-substring-no-properties
                               (point-min) mevedel-view--input-marker)))
                (should (string-match-p "UserPromptSubmit" expanded))
                (should (string-match-p "changed by test hook" expanded))
                (should (string-match-p "Original prompt:" expanded))
                (should (string-match-p "original prompt" expanded))
                (should (string-match-p "Submitted prompt:" expanded))
                (should (string-match-p "rewritten prompt" expanded)))))
          (with-current-buffer data-buf
            (let ((text (buffer-string)))
              (should (string-match-p "<!-- mevedel-hook-audit -->" text))
              (should (string-match-p "rewritten prompt" text))
              (goto-char (point-min))
              (search-forward "<!-- mevedel-hook-audit -->")
              (should (eq 'ignore
                          (get-text-property (match-beginning 0)
                                             'gptel)))))
          (with-current-buffer view-buf
            (mevedel-view--full-rerender)
            (let ((text (buffer-substring-no-properties
                         (point-min) mevedel-view--input-marker)))
              (should (string-match-p "hook changed prompt" text))
              (should-not (string-match-p "original prompt" text)))))
      (delete-directory root t)))

  :doc "inline skill Prompt omits hook context in immediate render"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myskill"
       :body "Expanded $0"
       :context 'inline
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook-with-context)))))))
          send-called)
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myskill hello")
          (mevedel-view-send)
          (should send-called)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "\\$myskill hello" text))
            (should (string-match-p "Prompt" text))
            (should (string-match-p "hook context added" text))
            (should-not (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "model-only context" text)))
          (goto-char (point-min))
          (search-forward "Prompt")
          (mevedel-view-toggle-section)
          (let ((expanded (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
            (should (string-match-p "rewritten prompt" expanded))
            (should-not (string-match-p "model-only context" expanded)))))
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "rewritten prompt" text))
          (should (string-match-p "model-only context" text))))))

  :doc "rewritten fork skill prompt sends as normal prompt without invoking skill"
  (mevedel-view-test--with-fork-skill
      (mevedel-skill--create
       :name "myfork"
       :body "ignored"
       :context 'fork
       :agent "general-purpose"
       :user-invocable-p t)
    (let ((mevedel-hook-rules
           '((UserPromptSubmit
              ((:matcher "*"
                         :hooks ((:type elisp
                                        :function
                                        mevedel-view-test--rewrite-prompt-hook)))))))
          invoke-called
          send-called)
      (cl-letf (((symbol-function 'mevedel-skills-invoke)
                 (lambda (&rest _)
                   (setq invoke-called t)))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _)
                   (setq send-called t))))
        (with-current-buffer view-buf
          (goto-char (mevedel-view--input-start))
          (insert "$myfork original")
          (mevedel-view-send))
        (should send-called)
        (should-not invoke-called)
        (with-current-buffer data-buf
          (let ((text (mevedel--strip-hook-audit-blocks
                       (buffer-string))))
            (should (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "\\$myfork original" text)))))))

  :doc "malformed UserPromptSubmit decisions are ignored"
  (let* ((root (make-temp-file "mevedel-view-hooks-malformed" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-malformed" root
                     "view-hooks-malformed"))
         (session (mevedel-session-create "main" workspace root))
         send-called)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                     (lambda (_event _event-plist callback &rest _)
                       (funcall callback :args)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "prompt with malformed hook result")
              (mevedel-view-send))
            (should send-called)))
      (delete-directory root t)))

  :doc "symbol UserPromptSubmit decisions are ignored"
  (let* ((root (make-temp-file "mevedel-view-hooks-symbol" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-symbol" root
                     "view-hooks-symbol"))
         (session (mevedel-session-create "main" workspace root))
         send-called)
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                     (lambda (_event _event-plist callback &rest _)
                       (funcall callback 'passed)))
                    ((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (setq send-called t))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "prompt with symbol hook result")
              (mevedel-view-send))
            (should send-called)))
      (delete-directory root t)))

  :doc "slow UserPromptSubmit command keeps the send path non-reentrant"
  (let* ((root (make-temp-file "mevedel-view-hooks-pending" t))
         (workspace (mevedel-workspace-get-or-create
                     'project "view-hooks-pending" root "view-hooks-pending"))
         (session (mevedel-session-create "main" workspace root))
         (mevedel-hook-rules
          '((UserPromptSubmit
             ((:matcher "*"
                        :hooks ((:type command
                                       :command "sleep 0.2; printf '{}'"
                                       :timeout 5)))))))
         (send-count 0))
    (unwind-protect
        (mevedel-view-test--with-buffers
          (with-current-buffer data-buf
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&rest _)
                       (cl-incf send-count))))
            (with-current-buffer view-buf
              (goto-char (mevedel-view--input-start))
              (insert "slow prompt")
              (mevedel-view-send)
              (should mevedel-view--prompt-hook-pending)
              (should-error (mevedel-view-send) :type 'user-error)
              (let ((deadline (+ (float-time) 5)))
                (while (and mevedel-view--prompt-hook-pending
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))
              (should-not mevedel-view--prompt-hook-pending)
              (should (= send-count 1)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-view--scaffolding-only-p ()
  ,test
  (test)
  :doc "marker-only and blank-only segments are scaffolding"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "\n#+end_tool\n\n#+begin_tool (Bash :command \"echo hi\")\n"))
    (should (mevedel-view--scaffolding-only-p
             data-buf (point-min) (with-current-buffer data-buf (point-max)))))

  :doc "real reasoning content is not scaffolding"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_reasoning\nLet me think about this.\n#+end_reasoning\n"))
    (should-not (mevedel-view--scaffolding-only-p
                 data-buf (point-min) (with-current-buffer data-buf (point-max))))))

(mevedel-deftest mevedel-view--response-summary ()
  ,test
  (test)
  :doc "keeps org source block markers in response summaries"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n"
     'response)
    (let ((summary (mevedel-view--response-summary
                    data-buf
                    (with-current-buffer data-buf (point-min))
                    (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "#\\+begin_src emacs-lisp" summary))
      (should-not (string-match-p "```emacs-lisp" summary)))))

(mevedel-deftest mevedel-view--user-turn-text/drawer-strip ()
  ,test
  (test)
  :doc "leading :PROPERTIES: drawer is stripped from user turn text"
  ;; Without the strip, gptel-org's per-buffer state drawer (system
  ;; prompt, model, GPTEL_BOUNDS) would render verbatim inside the
  ;; visible "You" turn on a full rerender that didn't pre-narrow past
  ;; the drawer.
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert ":PROPERTIES:\n:GPTEL_SYSTEM: hidden system prompt\n:END:\n")
      (insert "Real user prompt here.\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (mevedel-view--user-turn-text (list seg) data-buf)))
	  (should (string-match-p "Real user prompt" text))
	  (should-not (string-match-p "GPTEL_SYSTEM" text))
	  (should-not (string-match-p "hidden system prompt" text))))

  :doc "hook context blocks are stripped from visible user turn text"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "Real user prompt here.\n\n")
      (insert "<hook-context>\n")
      (insert "Model-only context.\n")
      (insert "</hook-context>\n"))
    (let* ((seg (list 'user (point-min)
                      (with-current-buffer data-buf (point-max))))
           (text (mevedel-view--user-turn-text (list seg) data-buf)))
      (should (string-match-p "Real user prompt" text))
      (should-not (string-match-p "hook-context" text))
      (should-not (string-match-p "Model-only context" text))))

  :doc "hook context renders as a collapsible view-only disclosure"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat "Real user prompt here.\n\n"
             "<hook-context>\n"
             "<hook-event name=\"UserPromptSubmit\">\n"
             "Model-only context.\n"
             "</hook-event>\n"
             "</hook-context>\n")
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "Real user prompt" text))
        (should (string-match-p "◇ hook context added" text))
        (should-not (string-match-p "Model-only context" text)))
      (goto-char (point-min))
      (search-forward "hook context added")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "UserPromptSubmit" text))
        (should (string-match-p "Model-only context" text)))))

  :doc "event-tagged hook context renders one combined disclosure"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     (concat
      "Real user prompt here.\n\n"
      "<hook-context>\n"
      "<hook-event name=\"SessionStart\">\n"
      "Startup context.\n"
      "</hook-event>\n"
      "<hook-event name=\"UserPromptSubmit\">\n"
      "Prompt context.\n"
      "</hook-event>\n"
      "</hook-context>\n")
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (= 1 (mevedel-view-test--count-substring
                      "hook context added" text)))
        (should (string-match-p "Real user prompt" text))
        (should-not (string-match-p "Startup context" text))
        (should-not (string-match-p "Prompt context" text)))
      (goto-char (point-min))
      (search-forward "hook context added")
      (mevedel-view-toggle-section)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "SessionStart" text))
        (should (string-match-p "Startup context" text))
        (should (string-match-p "UserPromptSubmit" text))
        (should (string-match-p "Prompt context" text))
        (should-not (string-match-p "<hook-event" text)))))

  :doc "escaped hook context preserves delimiter-looking body text"
  (mevedel-view-test--with-buffers
    (let ((body "literal </hook-event> & <tag>"))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "Real user prompt here.\n\n"
        (mevedel-hooks-format-context
         (list (list :event "UserPromptSubmit"
                     :body body)))
        "\n")
       nil)
      (with-current-buffer data-buf
        (let ((text (buffer-string)))
          (should (string-match-p "&lt;/hook-event&gt;" text))
          (should-not (string-match-p "literal </hook-event>" text))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Real user prompt" text))
          (should (string-match-p "hook context added" text))
          (should-not (string-match-p "literal </hook-event>" text)))
        (goto-char (point-min))
        (search-forward "hook context added")
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "UserPromptSubmit" text))
          (should (string-match-p (regexp-quote body) text))
          (should-not (string-match-p "&lt;/hook-event&gt;" text)))))))

(mevedel-deftest mevedel-view--render-mailbox-block
  (:doc "renders pure mailbox deliveries as message cards")
  ,test
  (test)

  :doc "pure agent-message turn renders without a You header"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-message from=\"explorer--abc123\">\nhello\n</agent-message>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✉ message from explorer--abc123" text))
        (should (string-match-p "hello" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))))

  :doc "pure agent-result turn renders with the same mailbox card path"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-result agent-id=\"worker--xyz789\" type=\"worker\">\nresult\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✓ finished worker--xyz789" text))
        (should (string-match-p "│ result" text))
        (should (string-match-p "result" text))
        (should (string-match-p "Assistant\n" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))
      (goto-char (point-min))
      (search-forward "│")
      (should (eq (get-text-property
                   (match-beginning 0) 'font-lock-face)
                  'mevedel-view-mailbox-gutter))
      (search-forward "result")
      (should (eq (get-text-property
                   (match-beginning 0) 'font-lock-face)
                  'mevedel-view-mailbox-body))))

  :doc "agent-result body may mention nested result blocks"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result agent-id=\"verifier--nested\" type=\"verifier\">\n"
        "Before nested example.\n"
        "```elisp\n"
        "(:body \"<agent-result>\n"
        "partial result\n"
        "</agent-result>\")\n"
        "```\n"
        "After nested example.\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ finished verifier--nested" text))
          (should (string-match-p "Before nested example" text))
          (should (string-match-p "After nested example" text))
          (should (string-match-p "partial result" text))
          (should-not (string-match-p "<agent-result agent-id=\"verifier--nested\""
                                      text))
          (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text))))))

  :doc "mailbox blocks separated by prose render independently"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result agent-id=\"reviewer--one\" type=\"reviewer\">\n"
        "first result\n"
        "</agent-result>\n"
        "Assistant prose between mailbox cards.\n"
        "<agent-result agent-id=\"verifier--two\" type=\"verifier\">\n"
        "second result\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (finished-count
                (cl-count-if (lambda (line)
                               (string-prefix-p "✓ finished" line))
                             (split-string text "\n"))))
          (should (= 2 finished-count))
          (should (string-match-p "✓ finished reviewer--one" text))
          (should (string-match-p "✓ finished verifier--two" text))
          (should (string-match-p "Assistant prose between mailbox cards" text))
          (should-not (string-match-p "<agent-result" text))))))

  :doc "indented mailbox close line is removed structurally"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result agent-id=\"worker--indented\" type=\"worker\">\nresult\n  </agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ finished worker--indented" text))
          (should (string-match-p "result" text))
          (should-not (string-match-p "</agent-result>" text))))))

  :doc "expanded agent-result keeps gutter on blank body lines"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-result agent-id=\"worker--blank\" type=\"worker\">\nfirst\n\nsecond\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p
                 "✓ finished worker--blank\n\n    │ first"
                 text))
        (should (string-match-p "│ first\n    │ \n    │ second" text)))))

  :doc "legacy agent-result from attribute renders as a mailbox card"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "<agent-result from=\"reviewer--abc123\">\nfindings\n</agent-result>\n"
     nil)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "✓ finished reviewer--abc123" text))
        (should (string-match-p "findings" text))
        (should-not (string-match-p "<agent-result" text))
        (should-not (string-match-p "\\`\\(?:.\\|\n\\)*You\n" text)))))

  :doc "long agent-result delivery expands to the final response body"
  (mevedel-view-test--with-buffers
    (let* ((mevedel-view-mailbox-collapse-line-threshold 1)
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "mailbox-long"
                       :root temporary-file-directory
                       :name "mailbox-long"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-save-path session)
            (file-name-as-directory
             (file-name-concat temporary-file-directory
                               "mevedel-mailbox-long-session")))
      (setf (mevedel-session-agent-transcripts session)
            '(("worker--long" . (:path "agents/worker--long.chat.org"
                                :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result agent-id=\"worker--long\" type=\"worker\">\nline one\nline two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✓ finished worker--long" text))
          (should (string-match-p
                   "✓ finished worker--long \\[[0-9]+ lines collapsed\\]"
                   text))
          (should-not (string-match-p
                       "✓ finished worker--long\n[[:space:]]+\\[[0-9]+ lines collapsed\\]"
                       text))
          (goto-char (point-min))
          (search-forward "line two")
          (should (eq (get-text-property (match-beginning 0) 'invisible)
                      'mevedel-view-mailbox-collapsed)))
        (goto-char (point-min))
        (search-forward "✓ finished worker--long")
        (goto-char (match-beginning 0))
        (search-forward "worker--long")
        (goto-char (match-beginning 0))
        (let (opened)
          (cl-letf (((symbol-function
                      'mevedel-view--open-agent-transcript-or-message)
                     (lambda (id &rest _) (setq opened id))))
            (mevedel-view-open-agent-transcript-at-point))
          (should (equal "worker--long" opened)))
        (goto-char (point-min))
        (search-forward "✓ finished worker--long")
        (goto-char (match-beginning 0))
        (mevedel-view-toggle-section)
        (goto-char (point-min))
        (search-forward "line two")
        (should-not (get-text-property (match-beginning 0) 'invisible))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ finished worker--long \\[[0-9]+ lines collapsed\\]"
                   text))
          (should-not (string-match-p
                       "✓ finished worker--long\n[[:space:]]+\\[[0-9]+ lines collapsed\\]"
                       text))))))

  :doc "collapsed agent-result counts non-empty payload lines"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 0))
      (mevedel-view-test--insert-data
       data-buf
       "<agent-result agent-id=\"worker--one\" type=\"worker\">\nresult\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p
                   "✓ finished worker--one \\[1 line collapsed\\]"
                   text))
          (should-not (string-match-p "2 lines collapsed" text))))))

  :doc "mailbox decoration clears inherited agent-handle properties"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "mailbox-stale"
                       :root temporary-file-directory
                       :name "mailbox-stale"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-save-path session)
            (file-name-as-directory
             (file-name-concat temporary-file-directory
                               "mevedel-mailbox-stale-session")))
      (setf (mevedel-session-agent-transcripts session)
            '(("explorer--stale" . (:path "agents/explorer--stale.chat.org"
                                  :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)))
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nlaunch\n"))
    (with-current-buffer view-buf
      (let* ((stale-source (cons 1 (with-current-buffer data-buf (point-max))))
             (start nil))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (progn
                (setq start (point))
                (insert "<agent-result agent-id=\"explorer--stale\" type=\"explorer\">\nfinal body\n</agent-result>\n")
                (add-text-properties
                 start (point)
                 `(mevedel-view-source ,stale-source
                   mevedel-view-type agent-handle
                   mevedel-view-agent-id "explorer--stale"
                   mevedel-view-agent-handle-p t
                   mevedel-view-agent-status completed))
                (mevedel-view--decorate-agent-result-blocks start (point)))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (goto-char start)
        (search-forward "✓ finished explorer--stale")
        (search-backward "explorer--stale")
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'mevedel-view-source))
        (should-not (get-text-property (point) 'mevedel-view-agent-handle-p))
        (should (equal "explorer--stale"
                       (get-text-property (point) 'mevedel-view-agent-id)))
        (search-forward "final body")
        (should-not (get-text-property (match-beginning 0)
                                       'mevedel-view-agent-id)))))

  :doc "mailbox delivery between response chunks stays in one assistant turn"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "Before mailbox.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf
     "\n<agent-message from=\"explorer\">\nhello\n</agent-message>\n\n"
     nil)
    (mevedel-view-test--insert-data data-buf "After mailbox.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let* ((text (buffer-substring-no-properties
                    (point-min) mevedel-view--input-marker))
             (assistant-count
              (cl-count-if (lambda (line) (string= line "Assistant"))
                           (split-string text "\n"))))
        (should (= 1 assistant-count))
        (should (string-match-p "Before mailbox" text))
        (should (string-match-p "✉ message from explorer" text))
        (should (string-match-p "hello" text))
        (should (string-match-p "After mailbox" text)))))

  :doc "agent-result after response does not render as a You turn"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 200))
      (mevedel-view-test--insert-data
       data-buf
       "Reviewer returned clean. Waiting on verifier.\n"
       'response)
      (mevedel-view-test--insert-data
       data-buf
       (concat
        "<agent-result agent-id=\"verifier--ar\" type=\"verifier\">\n"
        "Output observed:\n"
        "```elisp\n"
        "(:body \"<agent-result agent-id=\\\"explorer--A\\\">\n"
        "partial\n"
        "</agent-result>\")\n"
        "```\n"
        "VERDICT: FAIL\n"
        "</agent-result>\n")
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "Reviewer returned clean" text))
          (should (string-match-p "✓ finished verifier--ar" text))
          (should (string-match-p "VERDICT: FAIL" text))
          (should-not (string-match-p
                       "You\n✓ finished verifier--ar"
                       text))))))

  :doc "mailbox toggle does not expand a preceding Agent source"
  (mevedel-view-test--with-buffers
    (let ((mevedel-view-mailbox-collapse-line-threshold 1))
      (mevedel-view-test--insert-data
       data-buf
       "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"Skim mevedel-queue.el\"))\n\nAgent launched in background\n"
       '(tool . "call_agent"))
      (mevedel-view-test--insert-data
       data-buf
       "Assistant text before mailbox.\n"
       'response)
      (mevedel-view-test--insert-data
       data-buf
       "\n<agent-message from=\"explorer\">\nHello from your Explorer Agent :)\n</agent-message>\n\n<agent-result agent-id=\"explorer--33d949f0\" type=\"explorer\">\nfinal line one\nfinal line two\n</agent-result>\n"
       nil)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "✓ finished explorer--33d949f0")
        (goto-char (match-beginning 0))
        (should (eq (get-text-property (point) 'mevedel-view-type)
                    'mailbox-delivery))
        (should-not (get-text-property (point) 'mevedel-view-source))
        (mevedel-view-toggle-section)
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✉ message from explorer" text))
          (should (string-match-p "Hello from your Explorer Agent :)" text))
          (should (string-match-p "✓ finished explorer--33d949f0" text))
          (should (string-match-p "final line two" text))
          (should-not (string-match-p "Skim mevedel-queue.el (370 lines)"
                                      text)))))))

(mevedel-deftest mevedel-view-open-agent-transcript-at-point
  (:doc "opens transcript at attribution targets")
  ,test
  (test)

  :doc "insert-attribution stamps clickable id with transcript property"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr"
                       :root temporary-file-directory
                       :name "attr"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-session"))))
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abc123.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id))
               (pos (string-match-p "explorer--abc123" s)))
          (should pos)
          (should (equal agent-id
                         (get-text-property pos 'mevedel-view-agent-id s)))))))

  :doc "short display ids resolve to canonical transcript entries"
  (mevedel-view-test--with-buffers
    (let* ((canonical "explorer--abcdef0123456789abcdef0123456789")
           (short "explorer--abcdef01")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-short"
                       :root temporary-file-directory
                       :name "attr-short"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-short-session"))))
      (unwind-protect
          (progn
            (make-directory (file-name-concat save-path "agents") t)
            (write-region "" nil
                          (file-name-concat
                           save-path
                           "agents/explorer--abcdef01.chat.org")
                          nil 'silent)
            (setf (mevedel-session-save-path session) save-path)
            (setf (mevedel-session-agent-transcripts session)
                  (list (cons canonical
                              '(:path "agents/explorer--abcdef01.chat.org"
                                :status completed))))
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer view-buf
              (let ((entry (mevedel-view--lookup-transcript-entry short))
                    (info (mevedel-view--resolve-agent-transcript short)))
                (should (equal "agents/explorer--abcdef01.chat.org"
                               (plist-get entry :path)))
                (should (equal canonical (plist-get info :agent-id))))))
        (when (file-directory-p save-path)
          (delete-directory save-path t)))))

  :doc "running transcript attribution is clickable and reports unavailable live buffer"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-running"
                       :root temporary-file-directory
                       :name "attr-running"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-running-session")))
           message-text)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abc123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id nil 7))
               (pos (string-match-p "explorer--abc123" s)))
          (should pos)
          (should (get-text-property pos 'keymap s))
          (should (equal agent-id
                         (get-text-property pos 'mevedel-view-agent-id s)))
          (let ((inhibit-read-only t)
                start)
            (goto-char mevedel-view--input-marker)
            (setq start (point))
            (insert s)
            (goto-char (+ start pos)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (mevedel-view-open-agent-transcript-at-point))
          (should (string-match-p "Live buffer unavailable" message-text))
          (should (string-match-p "7 tool calls" message-text)))))))

  :doc "terminal live status overrides stale running sidecar for attribution"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--race123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-race"
                       :root temporary-file-directory
                       :name "attr-race"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-race-session")))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'completed))
           opened
           message-text)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--race123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (_id) inv))
                  ((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-text (apply #'format fmt args)))))
          (mevedel-view--open-agent-transcript-or-message agent-id)
          (should (equal agent-id opened))
          (should-not message-text)))))

  :doc "read-only attach reports unavailable live buffer for running transcripts"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (root (file-name-as-directory
                  (make-temp-file "mevedel-attr-readonly" t)))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-readonly"
                       :root root
                       :name "attr-readonly"))
           (session (mevedel-session-create "main" workspace))
           opened
           message-text)
      (setf (mevedel-session-save-path session) root)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abc123.chat.org"
                          :status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-session--read-only-mode t))
      (with-current-buffer view-buf
        (let* ((s (mevedel-view--insert-attribution agent-id nil 4))
               (pos (string-match-p "explorer--abc123" s)))
          (let ((inhibit-read-only t)
                start)
            (goto-char mevedel-view--input-marker)
            (setq start (point))
            (insert s)
            (goto-char (+ start pos)))
          (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                     (lambda (&rest _) (setq opened t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
          (mevedel-view-open-agent-transcript-at-point))
        (should-not opened)
        (should (string-match-p "Live buffer unavailable" message-text))))))

  :doc "survives display-region keymap overlay by using agent-id property"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (opened nil)
           (s (copy-sequence "from explorer--abc123")))
      (add-text-properties (length "from ") (length s)
                           `(mevedel-view-agent-id ,agent-id
                             keymap ,(make-sparse-keymap)
                             help-echo "Open transcript")
                           s)
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert s)
                (add-text-properties
                 start (point)
                 `(read-only t keymap ,mevedel-view--display-map
                   front-sticky (read-only keymap)
                   rear-nonsticky (read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (goto-char (point-min))
        (search-forward "explorer--abc123")
        (goto-char (match-beginning 0))
        (should (equal agent-id
                       (get-text-property (point) 'mevedel-view-agent-id)))
        (should (eq (get-text-property (point) 'keymap)
                    mevedel-view--display-map))
        (should (eq (lookup-key mevedel-view--display-map [mouse-2])
                    #'mevedel-view-activate-at-point))
        (cl-letf (((symbol-function
                    'mevedel-view--open-agent-transcript-or-message)
                   (lambda (id &rest _) (setq opened id))))
          (mevedel-view-activate-at-point)
          (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--agent-transcript-setup
  (:doc "sets up transcript inspection views without chat input zones")
  ,test
  (test)

  :doc "transcript view has co-located hidden markers and no prompt"
  (let ((data-buf (generate-new-buffer " *test-agent-data*"))
        (view-buf (generate-new-buffer " *test-agent-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           view-buf data-buf
           '(:agent-transcript-p t
             :agent-id "explorer--abc123"
             :transcript-info (:agent-id "explorer--abc123"
                               :status completed
                               :calls 2
                               :elapsed 1.5
                               :session-label "main")))
          (with-current-buffer view-buf
            (should mevedel-view--agent-transcript-p)
            (should (equal "explorer--abc123" mevedel-view--agent-id))
            (should (eq (lookup-key (current-local-map) (kbd "q"))
                        #'mevedel-view-close-agent-transcript))
            (should-not (eq (lookup-key mevedel-view-mode-map (kbd "q"))
                            #'mevedel-view-close-agent-transcript))
            (should (= (point-min) (marker-position mevedel-view--input-marker)))
            (should (= (marker-position mevedel-view--status-marker)
                       (marker-position mevedel-view--input-marker)))
            (should (= (marker-position mevedel-view--interaction-marker)
                       (marker-position mevedel-view--input-marker)))
            (should-not (string-match-p "> " (buffer-string)))
            (should (string-match-p "Agent explorer--abc123"
                                    (mevedel-view--agent-transcript-header-line)))
            (should-error (mevedel-view-send) :type 'user-error)
            (should-error (mevedel-view-abort) :type 'user-error)))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view-agent-handle-activate
  (:doc "dispatches agent handles to transcript open when available")
  ,test
  (test)

  :doc "running handle opens a live transcript when an invocation exists"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle"
                       :root temporary-file-directory
                       :name "handle"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'running))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (should (eq 'running
                    (plist-get
                     (mevedel-view--lookup-transcript-entry agent-id)
                     :status)))
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (_id) inv))
                  ((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t)))
                  ((symbol-function 'mevedel-view--full-rerender)
                   (lambda () nil)))
          (mevedel-view-agent-handle-activate)
          (should opened))))))

  :doc "terminal handle opens rendered transcript path"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle-terminal"
                       :root temporary-file-directory
                       :name "handle-terminal"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view-agent-handle-activate)
          (should (equal agent-id opened))))))

  :doc "terminal status wins when a running handle races completion"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle-race"
                       :root temporary-file-directory
                       :name "handle-race"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "Agent line\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t
                              'mevedel-view-agent-status 'running)))
        (goto-char (point-min))
        (search-forward "Agent line")
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view-agent-handle-activate)
          (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--agent-transcript-window
  (:doc "manages the singleton transcript side window")
  ,test
  (test)

  :doc "new transcript reuses the prior singleton and kills the previous view"
  (let ((parent (generate-new-buffer " *test-parent-view*"))
        (old-data (generate-new-buffer " *test-old-agent-data*"))
        (old-view (generate-new-buffer " *test-old-agent-view*"))
        (new-data (generate-new-buffer " *test-new-agent-data*"))
        (new-view (generate-new-buffer " *test-new-agent-view*"))
        reused-window)
    (unwind-protect
        (progn
          (with-current-buffer old-data
            (org-mode))
          (mevedel-view--setup
           old-view old-data
           (list :agent-transcript-p t
                 :agent-id "explorer--old"
                 :parent-view parent))
          (with-current-buffer new-data
            (org-mode))
          (mevedel-view--setup
           new-view new-data
           (list :agent-transcript-p t
                 :agent-id "explorer--new"
                 :parent-view parent))
          (with-current-buffer new-view
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "\ntranscript tail")))
          (with-current-buffer parent
            (mevedel-view-mode)
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window))
            (set-window-buffer (selected-window) old-view)
            (setq reused-window (selected-window))
            (mevedel-view--display-agent-transcript-view new-view)
            (should (eq reused-window mevedel-view--agent-transcript-window))
            (should (eq (window-buffer reused-window) new-view))
            (should (= (window-point reused-window)
                       (with-current-buffer new-view (point-max))))
            (should-not (buffer-live-p old-view))
            (should-not (buffer-live-p old-data))
            (should (window-live-p mevedel-view--agent-transcript-window)))
          (kill-buffer new-view)
          (with-current-buffer parent
            (should-not mevedel-view--agent-transcript-window)))
      (when (buffer-live-p new-view) (kill-buffer new-view))
      (when (buffer-live-p new-data) (kill-buffer new-data))
      (when (buffer-live-p old-view) (kill-buffer old-view))
      (when (buffer-live-p old-data) (kill-buffer old-data))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "killing the parent view kills open terminal transcript buffers"
  (let ((parent-data (generate-new-buffer " *test-parent-data-close*"))
        (parent-view (generate-new-buffer " *test-parent-view-close*"))
        (agent-data (generate-new-buffer " *test-agent-data-close*"))
        (agent-view (generate-new-buffer " *test-agent-view-close*")))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           agent-view agent-data
           (list :agent-transcript-p t
                 :agent-id "explorer--close"
                 :parent-view parent-view
                 :transcript-info
                 (list :agent-id "explorer--close"
                       :status 'completed)))
          (with-current-buffer parent-view
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window)))
          (kill-buffer parent-view)
          (should-not (buffer-live-p parent-view))
          (should-not (buffer-live-p parent-data))
          (should-not (buffer-live-p agent-view))
          (should-not (buffer-live-p agent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "killing a live transcript data buffer kills only its inspection view"
  (let ((parent-data (generate-new-buffer " *test-parent-data-live-close*"))
        (parent-view (generate-new-buffer " *test-parent-view-live-close*"))
        (agent-data (generate-new-buffer " *test-agent-data-live-close*"))
        (agent-view (generate-new-buffer " *test-agent-view-live-close*")))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local default-directory temporary-file-directory))
          (mevedel-view--setup
           agent-view agent-data
           (list :agent-transcript-p t
                 :agent-id "explorer--live-close"
                 :parent-view parent-view
                 :preserve-data-view-buffer t
                 :transcript-info
                 (list :agent-id "explorer--live-close"
                       :status 'running
                       :live-buffer t)))
          (kill-buffer agent-data)
          (should-not (buffer-live-p agent-data))
          (should-not (buffer-live-p agent-view))
          (should (buffer-live-p parent-view))
          (should (buffer-live-p parent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data)
        (with-current-buffer agent-data
          (setq kill-buffer-hook nil))
        (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "pop-to-buffer fallback stores the selected window, not its buffer"
  (let ((parent (generate-new-buffer " *test-parent-fallback-view*"))
        (data (generate-new-buffer " *test-agent-fallback-data*"))
        (view (generate-new-buffer " *test-agent-fallback-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data
            (org-mode))
          (mevedel-view--setup
           view data
           (list :agent-transcript-p t
                 :agent-id "explorer--fallback"
                 :parent-view parent))
          (with-current-buffer parent
            (mevedel-view-mode)
	            (cl-letf (((symbol-function 'display-buffer)
	                       (lambda (&rest _) (error "Display failed")))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (buf &rest _)
                         (set-window-buffer (selected-window) buf)
                         buf)))
              (mevedel-view--display-agent-transcript-view view))
            (should (window-live-p mevedel-view--agent-transcript-window))
            (should (eq (window-buffer mevedel-view--agent-transcript-window)
                        view))))
      (when (buffer-live-p view) (kill-buffer view))
      (when (buffer-live-p data) (kill-buffer data))
      (when (buffer-live-p parent) (kill-buffer parent)))))

(mevedel-deftest mevedel-view-open-agent-transcript
  (:doc "opens terminal transcripts as rendered inspection views")
  ,test
  (test)

  :doc "terminal transcript opens rendered view and q kills view plus data buffer"
  (let* ((agent-id "explorer--abc123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-view" t)))
         (agents-dir (file-name-concat root "agents"))
         (rel-path "agents/explorer--abc123.chat.org")
         (abs-path (file-name-concat root rel-path))
         (data-buf (generate-new-buffer " *test-parent-data*"))
         (view-buf (generate-new-buffer " *test-parent-view*"))
         agent-view
         agent-data
         restored-bounds)
    (make-directory agents-dir t)
    (with-temp-file abs-path
      (insert ":PROPERTIES:\n"
              ":GPTEL_BOUNDS: ((response (42 55)))\n"
              ":END:\n"
              "*** Agent prompt\n"))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-view"
                           :root root :name "transcript-view"))
               (session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-save-path session) root)
          (setf (mevedel-session-agent-transcripts session)
                (list (cons agent-id
                            (list :path rel-path
                                  :status 'completed
                                  :calls 3
                                  :elapsed 2.5))))
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'gptel--restore-props)
                       (lambda (bounds)
                         (setq restored-bounds bounds))))
              (mevedel-view-open-agent-transcript agent-id)))
          (setq agent-view (get-buffer "*mevedel-agent:explorer--abc123*"))
          (should (buffer-live-p agent-view))
          (should (equal '((response (42 55))) restored-bounds))
          (with-current-buffer agent-view
            (setq agent-data mevedel--data-buffer)
            (should mevedel-view--agent-transcript-p)
            (should (string-match-p "3 calls"
                                    (mevedel-view--agent-transcript-header-line)))
            (mevedel-view-close-agent-transcript))
          (should-not (buffer-live-p agent-view))
          (should-not (buffer-live-p agent-data)))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))))

  :doc "running transcript opens live buffer and closing view leaves agent alive"
  (let* ((agent-id "explorer--live123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-live" t)))
         (parent-data (generate-new-buffer " *test-parent-data-live*"))
         (parent-view (generate-new-buffer " *test-parent-view-live*"))
         (agent-data (generate-new-buffer " *test-agent-data-live*"))
         agent-view
         parent-before)
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-live"
                           :root root :name "transcript-live"))
               (session (mevedel-session-create "main" workspace))
               (inv (mevedel-agent-invocation--create
                     :agent-id agent-id
                     :buffer agent-data
                     :transcript-status 'running
                     :call-count 4)))
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local default-directory root)
            (insert "*** Live agent prompt\n"))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer parent-view
            (setq parent-before
                  (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer parent-view
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv)))
              (mevedel-view-open-agent-transcript agent-id)))
          (with-current-buffer agent-data
            (should (eq mevedel--view-buffer parent-view)))
          (setq agent-view (get-buffer "*mevedel-agent:explorer--live123*"))
          (should (buffer-live-p agent-view))
          (with-current-buffer agent-view
            (should mevedel-view--agent-transcript-p)
            (should (plist-get mevedel-view--agent-transcript-info
                               :live-buffer))
            (should (string-search
                     "Live agent prompt"
                     (buffer-substring-no-properties
                      (point-min) (point-max))))
            (should (string-match-p "4 calls"
                                    (mevedel-view--agent-transcript-header-line)))
            (mevedel-view-close-agent-transcript))
          (should-not (buffer-live-p agent-view))
          (with-current-buffer parent-view
            (should (equal parent-before
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))
          (should (buffer-live-p agent-data))
          (with-current-buffer agent-data
            (should (eq mevedel--view-buffer parent-view))))
	      (when (buffer-live-p agent-view) (kill-buffer agent-view))
	      (when (buffer-live-p agent-data) (kill-buffer agent-data))
	      (when (buffer-live-p parent-view) (kill-buffer parent-view))
	      (when (buffer-live-p parent-data) (kill-buffer parent-data))
	      (when (file-directory-p root) (delete-directory root t))))

  :doc "running transcript view skips bounds repair for incomplete live blocks"
  (let* ((agent-id "verifier--partial-live")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-partial-live" t)))
         (parent-data (generate-new-buffer " *test-parent-data-partial-live*"))
         (parent-view (generate-new-buffer " *test-parent-view-partial-live*"))
         (agent-data (generate-new-buffer " *test-agent-data-partial-live*"))
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :buffer agent-data
               :transcript-status 'running
               :call-count 0))
         agent-view
         restored
         normalized)
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-partial-live"
                           :root root :name "transcript-partial-live"))
               (session (mevedel-session-create "main" workspace)))
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--agent-invocation inv)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local default-directory root)
            (insert "*** Live verifier prompt\n\n#+begin_reasoning\npartial"))
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer parent-view
            (cl-letf (((symbol-function 'display-buffer)
                       (lambda (buf _action)
                         (set-window-buffer (selected-window) buf)
                         (selected-window)))
                      ((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv))
                      ((symbol-function 'mevedel-view--restore-gptel-bounds)
                       (lambda () (setq restored t)))
                      ((symbol-function
                        'mevedel-session-persistence--normalize-gptel-properties)
                       (lambda () (setq normalized t))))
              (mevedel-view-open-agent-transcript agent-id)))
          (setq agent-view
                (get-buffer
                 (format "*mevedel-agent:%s*"
                         (mevedel-view--display-label-for-agent agent-id))))
          (should (buffer-live-p agent-view))
          (should-not restored)
          (should-not normalized)
          (with-current-buffer agent-view
            (should (string-search
                     "partial"
                     (buffer-substring-no-properties
                      (point-min) (point-max))))))
      (when (buffer-live-p agent-view) (kill-buffer agent-view))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "terminal live invocation status overrides stale running sidecar"
  (let* ((agent-id "explorer--race123")
         (root (file-name-as-directory
                (make-temp-file "mevedel-transcript-race" t)))
         (agents-dir (file-name-concat root "agents"))
         (rel-path "agents/explorer--race123.chat.org")
         (abs-path (file-name-concat root rel-path))
         (data-buf (generate-new-buffer " *test-parent-data-race*"))
         (view-buf (generate-new-buffer " *test-parent-view-race*"))
         (inv (mevedel-agent-invocation--create
               :agent-id agent-id
               :transcript-status 'completed)))
    (make-directory agents-dir t)
    (with-temp-file abs-path
      (insert "*** Agent prompt\n"))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create
                           :type 'project :id "transcript-race"
                           :root root :name "transcript-race"))
               (session (mevedel-session-create "main" workspace)))
          (setf (mevedel-session-save-path session) root)
          (setf (mevedel-session-agent-transcripts session)
                (list (cons agent-id
                            (list :path rel-path
                                  :status 'running))))
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local default-directory root))
          (mevedel-view--setup view-buf data-buf)
          (with-current-buffer view-buf
            (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                       (lambda (_id) inv)))
              (let ((info (mevedel-view--resolve-agent-transcript agent-id)))
                (should (eq 'completed (plist-get info :status)))
                (should (equal abs-path
                               (plist-get info :absolute-path)))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-view--agent-status-collect
  (:doc "derives aggregate rows from current rendered handles and sidecar")
  ,test
  (test)

  :doc "aggregate status leaves a blank line before the input prompt"
  (let ((text (mevedel-view--agent-status-string
               (list (list :agent-id "explorer--abc"
                           :status 'completed
                           :description "done"
                           :calls 1))
               nil)))
    (should (string-suffix-p "\n\n" text)))

  :doc "aggregate status toggle is attached to the suffix button only"
  (let* ((text (mevedel-view--agent-status-string
                (list (list :agent-id "explorer--abc"
                            :status 'running
                            :description "count"
                            :calls 1))
                nil))
         (button-pos (string-match-p (regexp-quote "[+]") text)))
    (should-not (lookup-key mevedel-view-mode-map (kbd "C-c C-a")))
    (should button-pos)
    (should (eq (lookup-key (get-text-property button-pos 'keymap text)
                            (kbd "RET"))
                #'mevedel-view-activate-at-point))
    (should (get-text-property button-pos 'follow-link text))
    (should-not (get-text-property (max 0 (1- button-pos))
                                   'keymap text)))

  :doc "display navigation moves through status fragments before the composer"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-fragment-navigation"
                       :root temporary-file-directory
                       :name "status-fragment-navigation"))
           (session (mevedel-session-create "main" workspace))
           (agent-id "explorer--nav123"))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "count"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (should (eq (lookup-key mevedel-view--display-map (kbd "n"))
                    #'mevedel-view-next-display))
        (should (eq (lookup-key mevedel-view--display-map (kbd "p"))
                    #'mevedel-view-previous-display))
        (should-not (lookup-key mevedel-view-mode-map (kbd "n")))
        (should-not (lookup-key mevedel-view-mode-map (kbd "RET")))
        (goto-char (point-min))
        (mevedel-view-next-display)
        (should (eq 'tasks (get-text-property
                            (point) 'mevedel-view-fragment-id)))
        (let ((map (get-text-property (point) 'keymap)))
          (should (eq (lookup-key map (kbd "n"))
                      #'mevedel-view-next-display))
          (should (eq (lookup-key map (kbd "p"))
                      #'mevedel-view-previous-display))
          (should (eq (lookup-key map (kbd "RET"))
                      #'mevedel-view-activate-at-point))
          (should-not (cdr (get-char-property-and-overlay
                            (point) 'mevedel-tool-task))))
        (mevedel-view-next-display)
        (should (eq 'agents (get-text-property
                             (point) 'mevedel-view-fragment-id)))
        (let ((last-fragment (point)))
          (mevedel-view-next-display)
          (should (= (point) last-fragment)))
        (mevedel-view-previous-display)
        (should (eq 'tasks (get-text-property
                            (point) 'mevedel-view-fragment-id))))))

  :doc "direct status render clears stale task compatibility overlay"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-task-overlay-cleanup"
                       :root temporary-file-directory
                       :name "status-task-overlay-cleanup"))
           (session (mevedel-session-create "main" workspace))
           legacy-overlay)
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq legacy-overlay (make-overlay (point-min) (point-min)))
        (setf (mevedel-session-task-overlay session) legacy-overlay))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (should (string-match-p "visible task" (buffer-string)))
        (should-not (overlay-buffer legacy-overlay))
        (should-not (mevedel-session-task-overlay session)))))

  :doc "display navigation chooses the next turn before later status fragments"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-fragment-nearest"
                       :root temporary-file-directory
                       :name "status-fragment-nearest"))
           (session (mevedel-session-create "main" workspace))
           first-start second-start)
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--status-marker)
          (setq first-start (point))
          (insert "First turn\n")
          (add-text-properties first-start (point)
                               `(read-only t
                                 keymap ,mevedel-view--display-map
                                 mevedel-view-source (1 . 10)))
          (setq second-start (point))
          (insert "Second turn\n")
          (add-text-properties second-start (point)
                               `(read-only t
                                 keymap ,mevedel-view--display-map
                                 mevedel-view-source (11 . 20)))
          (set-marker mevedel-view--status-marker (point))
          (set-marker mevedel-view--interaction-marker (point)))
        (mevedel-view--render-status data-buf)
        (goto-char first-start)
        (mevedel-view-next-display)
        (should (= (point) second-start))
        (mevedel-view-next-display)
        (should (eq 'tasks (get-text-property
                            (point) 'mevedel-view-fragment-id))))))

  :doc "shared activation refuses the editable composer"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-test--insert-composer-draft "draft text" 2)
      (should-error (mevedel-view-activate-at-point) :type 'user-error)
      (should (string= "draft text" (mevedel-view--input-text)))))

  :doc "agent status collapse is backed by fragment collapse state"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-fragment-collapse"
                       :root temporary-file-directory
                       :name "status-fragment-collapse"))
           (session (mevedel-session-create "main" workspace))
           (agent-id "explorer--collapse123"))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "count"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (goto-char (point-min))
        (search-forward "Agent: explorer -- count" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (should (eq 'agents (get-text-property
                             (point) 'mevedel-view-fragment-id)))
        (mevedel-view-toggle-section)
        (should (mevedel-view-fragment-collapse-state
                 mevedel-view--status-agent-collapse-key))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "1 agent: 1 running" text))
          (should-not (string-match-p "Agent: explorer -- count" text)))
        (goto-char (point-min))
        (search-forward "[+]" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (should (eq (lookup-key (get-text-property (point) 'keymap)
                                (kbd "RET"))
                    #'mevedel-view-activate-at-point))
        (mevedel-view-activate-at-point)
        (should-not (mevedel-view-fragment-collapse-state
                     mevedel-view--status-agent-collapse-key))
        (goto-char (point-min))
        (should (search-forward "Agent: explorer -- count"
                                mevedel-view--input-marker t)))))

  :doc "status fallback renders as a status Agent handle fragment"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((agent-id "explorer--fragment"))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id agent-id
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count" mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (should (eq (get-text-property (point) 'mevedel-view-type)
                      'agent-handle))
          (should (get-text-property (point) 'mevedel-view-agent-handle-p))
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-handle-map))
          (should-not (lookup-key (get-text-property (point) 'keymap)
                                  [mouse-1]))
          (should (eq 'status (get-text-property
                               (point) 'mevedel-view-fragment-namespace)))
          (should (eq 'agents (get-text-property
                               (point) 'mevedel-view-fragment-id)))))))

  :doc "status fallback leaves a blank line before request spinner"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                 (lambda ()
                   (list (list :agent-id "explorer--spinner"
                               :status 'running
                               :description "count"
                               :calls 1)))))
        (mevedel-view--render-agent-status)
        (mevedel-view--start-spinner "Working...")
        (let ((display (buffer-substring-no-properties
                        (point-min) (mevedel-view--input-start))))
          (should (string-match-p
                   "Agent: explorer -- count[^\n]*\n\n[^\n]*Working"
                   display))
          (should-not (string-match-p
                       "Agent: explorer -- count[^\n]*\n\n\n"
                       display)))
        (mevedel-view--stop-spinner))))

  :doc "status fallback preserves multiline composer text starting with >"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "explorer--draft"
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4)))
        (should-not (get-text-property (mevedel-view--input-start)
                                       'read-only))
        (save-excursion
          (let ((display (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
            (should (string-match-p "Agent: explorer -- count" display))
            (goto-char (point-min))
            (search-forward "Agent: explorer -- count"
                            mevedel-view--input-marker)
            (should (get-text-property (match-beginning 0) 'read-only)))))))

  :doc "status fallback preserves composer when all zone markers drift"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((draft "> quoted\nsecond line"))
        (goto-char (mevedel-view--input-start))
        (insert draft)
        (goto-char (+ (mevedel-view--input-start) 4))
        (set-marker mevedel-view--status-marker (point-max))
        (set-marker mevedel-view--interaction-marker (point-max))
        (set-marker mevedel-view--input-marker (point-max))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "explorer--drift"
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (mevedel-view-refresh-input-prompt)
        (should (string= draft (mevedel-view--input-text)))
        (save-excursion
          (let ((display (buffer-substring-no-properties
                          (point-min) (mevedel-view--input-start)))
                (input (buffer-substring-no-properties
                        (mevedel-view--input-start) (point-max))))
            (should (string-match-p "Agent: explorer -- count" display))
            (should-not (string-match-p "Agent: explorer -- count" input)))))))

  :doc "status fallback ignores stale data-buffer task overlays"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "stale-status-overlay"
                       :root temporary-file-directory
                       :name "stale-status-overlay"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (let ((ov (make-overlay (point-min) (point-min)
                                (current-buffer) t nil)))
          (overlay-put ov 'mevedel-tool-task t)
          (setf (mevedel-session-task-overlay session) ov))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "verifier--stale123"
                                 :status 'running
                                 :agent-type "verifier"
                                 :description "verify stale anchor"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (agent-pos (string-search
                           "Agent: verifier -- verify stale anchor"
                           text)))
          (should agent-pos)
          (should (>= (+ (point-min) agent-pos)
                      (marker-position mevedel-view--status-marker)))))))

  :doc "live status rows render below the task status fragment"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-below-tasks"
                       :root temporary-file-directory
                       :name "status-below-tasks"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-tasks session)
            (list (mevedel-task--create
                   :id 1 :subject "visible task" :status 'pending)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (mevedel-view--render-status data-buf)
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id "verifier--below123"
                                 :status 'running
                                 :agent-type "verifier"
                                 :description "verify changes"
                                 :calls 2)))))
          (mevedel-view--render-agent-status))
        (let* ((text (buffer-substring-no-properties
                      (point-min) mevedel-view--input-marker))
               (task-pos (string-match-p "visible task" text))
               (agent-pos (string-match-p
                           "Agent: verifier -- verify changes"
                           text)))
          (should task-pos)
          (should agent-pos)
          (should (< task-pos agent-pos))))))

  :doc "status fallback handles survive repeated refreshes"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--refresh123")
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "count"
                 :transcript-status 'running
                 :call-count 1
                 :buffer data-buf))
           (fake-fsm (gptel-make-fsm
                      :info (list :mevedel-agent-invocation inv)
                      :handlers nil
                      :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-refresh"
                       :root temporary-file-directory
                       :name "status-refresh"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-tools--agents-fsm
                    (list (cons agent-id fake-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-tools--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (mevedel-view--render-agent-status)
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count"
                          mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (should (get-text-property (point)
                                     'mevedel-view-agent-handle-p))))))

  :doc "status refresh does not delete history inserted at the boundary"
  (mevedel-view-test--with-buffers
    (let ((agent-id "explorer--boundary123"))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda ()
                     (list (list :agent-id agent-id
                                 :status 'running
                                 :description "count"
                                 :calls 1)))))
          (mevedel-view--render-agent-status))
        (goto-char mevedel-view--status-marker)
        (mevedel-view--with-render-boundaries-advancing
          (let ((inhibit-read-only t))
            (insert "Assistant transcript\n")))
        (cl-letf (((symbol-function 'mevedel-view--agent-status-collect)
                   (lambda () nil)))
          (mevedel-view--render-agent-status))
        (should (string-match-p
                 "Assistant transcript"
                 (buffer-substring-no-properties
                  (point-min) (point-max))))
        (should-not (string-match-p
                     "Agent: explorer -- count"
                     (buffer-substring-no-properties
                      (point-min) (point-max)))))))

  :doc "status fallback handles stay compact even with live activity"
  (mevedel-view-test--with-buffers
    (let* ((first-id "explorer--first123")
           (second-id "explorer--second456")
           (first-inv (mevedel-agent-invocation--create
                       :agent-id first-id
                       :description "count defvars"
                       :transcript-status 'running
                       :call-count 2
                       :activity '((:type waiting))
                       :buffer data-buf))
           (second-inv (mevedel-agent-invocation--create
                        :agent-id second-id
                        :description "count defcustoms"
                        :transcript-status 'running
                        :call-count 1
                        :activity '((:type waiting))
                        :buffer data-buf))
           (first-fsm (gptel-make-fsm
                       :info (list :mevedel-agent-invocation first-inv)
                       :handlers nil
                       :state 'WAIT))
           (second-fsm (gptel-make-fsm
                        :info (list :mevedel-agent-invocation second-inv)
                        :handlers nil
                        :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-sibling"
                       :root temporary-file-directory
                       :name "status-sibling"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-tools--agents-fsm
                    (list (cons first-id first-fsm)
                          (cons second-id second-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-tools--agent-invocation-at)
                   (lambda (fsm)
                     (cond
                      ((eq fsm first-fsm) first-inv)
                      ((eq fsm second-fsm) second-inv)))))
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count defvars"
                          mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (when-let* ((bounds (mevedel-view-fragment--bounds-at (point))))
            (let ((inhibit-read-only t))
              (put-text-property
               (plist-get bounds :start)
               (plist-get bounds :end)
               'mevedel-view-source
               (cons 1 1))))
          (should (equal first-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should (eq 'running
                      (get-text-property
                       (point) 'mevedel-view-agent-status)))
          (should (get-text-property (point) 'mevedel-view-source))
          (mevedel-view--render-agent-status)
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count defvars"
                          mevedel-view--input-marker)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p
                     "Agent: explorer -- count defvars"
                     text))
            (should-not
             (string-match-p
              "\n  … waiting"
              text)))))))

  :doc "status fallback handles truncate to one visual row"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "coordinator--wide123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-truncate"
                       :root temporary-file-directory
                       :name "status-truncate"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "coordinator"
                          :description "Run a bounded validation loop for current changes: tests, compile/checks, reviewer, verifier, fixes, then repeat until green"
                          :calls 14
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-status-line-width)
                   (lambda () 72)))
          (let* ((text (mevedel-view--agent-status-handles-string
                        (mevedel-view--agent-status-collect)))
                 (lines (split-string text "\n" t)))
            (should (= 1 (length lines)))
            (should (<= (string-width (car lines)) 72))
            (should (string-match-p
                     "Run a bounded .*\\.\\.\\.  \\[running · 14 calls\\]"
                     (car lines)))
            (should (string-match "coordinator" text))
            (should (eq (get-text-property
                         (match-beginning 0) 'keymap text)
                        mevedel-view--agent-label-map))
            (should (equal (get-text-property
                            (match-beginning 0)
                            'mevedel-view-agent-id text)
                           agent-id)))))))

  :doc "status row width uses the displayed view window"
  (mevedel-view-test--with-buffers
    (let ((other-buf (generate-new-buffer " *mevedel-test-other-window*")))
      (unwind-protect
          (progn
            (switch-to-buffer view-buf)
            (delete-other-windows)
            (let* ((view-window (selected-window))
                   (other-window (split-window-right)))
              (set-window-buffer other-window other-buf)
              (select-window other-window)
              (with-current-buffer view-buf
                (should (= (mevedel-view--agent-status-line-width)
                           (max 20
                                (1- (window-body-width
                                     view-window))))))))
        (delete-other-windows)
        (when (buffer-live-p other-buf)
          (kill-buffer other-buf)))))

  :doc "live nested invocations are reachable from parent session view"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef123456")
           (display-id "explorer--abcdef12")
           (agent-buf (generate-new-buffer " *test-nested-agent-live*"))
           (fake-fsm (cons 'nested 'fsm))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :buffer agent-buf
                 :transcript-status 'running))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "nested-live-lookup"
                       :root temporary-file-directory
                       :name "nested-live-lookup"))
           (session (mevedel-session-create "main" workspace)))
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local mevedel--session session))
            (with-current-buffer agent-buf
              (setq-local mevedel--session session)
              (setq-local mevedel-tools--agents-fsm
                          (list (cons agent-id fake-fsm))))
            (with-current-buffer view-buf
              (cl-letf (((symbol-function 'mevedel-tools--agent-invocation-at)
                         (lambda (fsm)
                           (and (eq fsm fake-fsm) inv))))
                (should (eq inv (mevedel-view--agent-invocation
                                 display-id))))))
        (when (buffer-live-p agent-buf)
          (kill-buffer agent-buf)))))

  :doc "live parent background-agent list orders child without sidecar metadata"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "reviewer--parent123")
           (child-id "explorer--child456")
           (parent-inv (mevedel-agent-invocation--create
                        :agent-id parent-id
                        :background-agents (list child-id)))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-live-parent"
                       :root temporary-file-directory
                       :name "status-live-parent"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons child-id
                        '(:status running
                          :agent-type "explorer"
                          :description "inspect current changes"
                          :parent-turn 1))
                  (cons parent-id
                        '(:status running
                          :agent-type "reviewer"
                          :description "review patch"
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (agent-id)
                     (and (equal agent-id parent-id) parent-inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (= 2 (length rows)))
            (should (equal parent-id (plist-get (nth 0 rows) :agent-id)))
            (should (equal child-id (plist-get (nth 1 rows) :agent-id)))
            (should (= 1 (plist-get (nth 1 rows) :depth))))))))

  :doc "omits agents whose handles are already visible in the current view"
  (mevedel-view-test--with-buffers
    (let* ((running-id "explorer--run123")
           (done-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-collect"
                       :root temporary-file-directory
                       :name "status-collect"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons running-id '(:status running :calls 1))
                  (cons done-id '(:status completed :calls 2))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "running\n"
                              'mevedel-view-agent-id running-id
                              'mevedel-view-agent-handle-p t))
          (insert (propertize "done\n"
                              'mevedel-view-agent-id done-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "stale queue origins do not promote terminal handles to blocked"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-stale"
                       :root temporary-file-directory
                       :name "status-stale"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "done\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "sidecar-running agent with queued interaction reports blocked"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--blocked123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-sidecar-blocked"
                       :root temporary-file-directory
                       :name "status-sidecar-blocked"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "running\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "sidecar-running queued agent reports blocked without rendered handle"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--sidecaronly123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-sidecar-only"
                       :root temporary-file-directory
                       :name "status-sidecar-only"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running :calls 1))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 1 (length rows)))
          (should (eq 'blocked (plist-get (car rows) :status)))))))

  :doc "sidecar-running nested agents appear even when parent handle is visible"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "coordinator--parent123")
           (child-id "reviewer--child456")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-nested-running"
                       :root temporary-file-directory
                       :name "status-nested-running"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons parent-id
                        '(:status running
                          :agent-type "coordinator"
                          :description "green loop"
                          :parent-turn 1))
                  (cons child-id
                        (list :status 'running
                              :agent-type "reviewer"
                              :description "review patch"
                              :parent-turn 1
                              :parent-agent-id parent-id))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "coordinator handle\n"
                              'mevedel-view-agent-id parent-id
                              'mevedel-view-agent-handle-p t)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 2 (length rows)))
          (should (equal parent-id (plist-get (nth 0 rows) :agent-id)))
          (should (equal child-id (plist-get (nth 1 rows) :agent-id)))
          (should (eq 'running (plist-get (nth 1 rows) :status)))
          (should (= 1 (plist-get (nth 1 rows) :depth))))
        (let ((text (mevedel-view--agent-status-handles-string
                     (mevedel-view--agent-status-collect))))
          (should (string-match-p "^  ● Agent: coordinator -- green loop"
                                  text))
          (should (string-match-p "^    ● Agent: reviewer -- review patch"
                                  text))))))

  :doc "live invocation parent context orders nested agents by actual parent"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "reviewer--parent123")
           (child-id "explorer--child456")
           (parent-inv (mevedel-agent-invocation--create
                        :agent-id parent-id
                        :description "review patch"))
           (child-inv (mevedel-agent-invocation--create
                       :agent-id child-id
                       :description "validate current changes"
                       :parent-context parent-inv))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-infer-parent"
                       :root temporary-file-directory
                       :name "status-infer-parent"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons child-id
                        '(:status running
                          :agent-type "explorer"
                          :description "validate current changes"
                          :parent-turn 1))
                  (cons parent-id
                        '(:status running
                          :agent-type "reviewer"
                          :description "review patch"
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (agent-id)
                     (cond
                      ((equal agent-id parent-id) parent-inv)
                      ((equal agent-id child-id) child-inv)))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (= 2 (length rows)))
            (should (equal parent-id (plist-get (nth 0 rows) :agent-id)))
            (should (equal child-id (plist-get (nth 1 rows) :agent-id)))
            (should (= 1 (plist-get (nth 1 rows) :depth))))))))

  :doc "running aggregate rows show call count without activity body"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--activity123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-activity"
                       :root temporary-file-directory
                       :name "status-activity"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "validate current changes"
                 :call-count 2
                 :activity (list (list :type 'waiting :summary "waiting")
                                 (list :type 'tool-finish
                                       :tool-name "Read"
                                       :summary "Read done")))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "validate current changes"
                          :parent-turn 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view--agent-invocation)
                   (lambda (_id) inv)))
          (let ((text (mevedel-view--agent-status-handles-string
                       (mevedel-view--agent-status-collect))))
            (should (string-match-p "\\[running · 2 calls\\]" text))
            (should-not (string-match-p "^  … waiting" text))
            (should-not (string-match-p "^  ✓ Read done" text)))))))

  :doc "current-turn completed nested agents do not produce aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((parent-id "coordinator--parent123")
           (child-id "verifier--child456")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-nested-done"
                       :root temporary-file-directory
                       :name "status-nested-done"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons child-id
                        (list :status 'completed
                              :agent-type "verifier"
                              :description "verify patch"
                              :calls 2
                              :parent-turn 1
                              :parent-agent-id parent-id))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "current-turn terminal sidecar entries do not produce aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-terminal-sidecar"
                       :root temporary-file-directory
                       :name "status-terminal-sidecar"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-turn-count session) 2)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons "verifier--done123"
                        (list :status 'completed
                              :agent-type "verifier"
                              :description "verify patch"
                              :calls 2
                              :parent-turn 3))
                  (cons "reviewer--aborted123"
                        (list :status 'aborted
                              :agent-type "reviewer"
                              :description "review task tooling diff"
                              :calls 3
                              :parent-turn 3
                              :reason "stopped by user"))
                  (cons "explorer--error123"
                        (list :status 'error
                              :agent-type "explorer"
                              :description "inspect changes"
                              :calls 4
                              :parent-turn 3
                              :reason "failed"))
                  (cons "coordinator--incomplete123"
                        (list :status 'incomplete
                              :agent-type "coordinator"
                              :description "coordinate"
                              :calls 5
                              :parent-turn 3))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "terminal live invocation suppresses stale running sidecar in aggregate"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--race123")
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'completed
                 :call-count 5
                 :buffer data-buf))
           (fake-fsm (gptel-make-fsm
                      :info (list :mevedel-agent-invocation inv)
                      :handlers nil
                      :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-terminal-race"
                       :root temporary-file-directory
                       :name "status-terminal-race"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running :calls 2))))
      (setf (mevedel-session-permission-queue session)
            (list (list :origin agent-id)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-tools--agents-fsm
                    (list (cons agent-id fake-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-tools--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (null rows)))))))

  :doc "old-turn errored registry entries are pruned from aggregate status"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "verifier--old-error123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-old-error"
                       :root temporary-file-directory
                       :name "status-old-error"))
           (session (mevedel-session-create "main" workspace))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "verify current diff"
                 :transcript-status 'error
                 :call-count 16))
           (fsm (gptel-make-fsm
                 :info (list :mevedel-agent-invocation inv)
                 :handlers nil
                 :state 'WAIT)))
      (setf (mevedel-session-turn-count session) 3)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status error
                          :agent-type "verifier"
                          :description "verify current diff"
                          :calls 16
                          :parent-turn 1
                          :reason "HTTP/2 503"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-tools--agents-fsm
                    (list (cons agent-id fsm))))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))
      (with-current-buffer data-buf
        (should-not (assoc agent-id mevedel-tools--agents-fsm)))))

  :doc "current-turn errored sidecar entries do not produce aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "verifier--current-error123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-current-error"
                       :root temporary-file-directory
                       :name "status-current-error"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status error
                          :agent-type "verifier"
                          :description "verify current diff"
                          :calls 16
                          :parent-turn 1
                          :reason "HTTP/2 503"))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (null rows))))))

  :doc "live agent without a visible handle still appears in aggregate status"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--hidden123")
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "hidden task"
                 :transcript-status 'running
                 :call-count 3
                 :buffer data-buf))
           (fake-fsm (gptel-make-fsm
                      :info (list :mevedel-agent-invocation inv)
                      :handlers nil
                      :state 'WAIT))
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-hidden-live"
                       :root temporary-file-directory
                       :name "status-hidden-live"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-tools--agents-fsm
                    (list (cons agent-id fake-fsm))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-tools--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (= 1 (length rows)))
            (should (eq 'running (plist-get (car rows) :status)))
            (should (= 3 (plist-get (car rows) :calls)))))))))

(mevedel-deftest mevedel-view-refresh-agent-rendering ()
  ,test
  (test)

  :doc "updates an expanded visible agent handle without full rerendering or changing draft"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--refresh123")
           (draft "> quoted\nsecond line")
           (agent-tool (mevedel-tool--create
                        :name "Agent"
                        :renderer #'mevedel-tool-ui--render-agent))
           bounds
           (render-data
            (list :kind 'agent-transcript
                  :agent-id agent-id
                  :status 'running
                  :calls 1
                  :background t)))
      (with-current-buffer data-buf
        (goto-char (point-max))
        (setq bounds (cons (point) nil))
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"count\"))\n")
        (insert "Agent is running.\n")
        (insert (mevedel-pipeline--format-render-data-block render-data))
        (setcdr bounds (point))
        (put-text-property (car bounds) (cdr bounds) 'gptel '(tool . "call-1")))
      (cl-letf (((symbol-function 'mevedel-tool-get)
                 (lambda (name &optional _category)
                   (and (equal name "Agent") agent-tool))))
        (with-current-buffer view-buf
          (let ((inhibit-read-only t))
            (goto-char mevedel-view--input-marker)
            (mevedel-view--render-tool-group
             (list (list 'tool (car bounds) (cdr bounds))) data-buf))
          (goto-char (point-min))
          (should (search-forward "[running · 1 calls]"
                                  mevedel-view--input-marker t))
          (goto-char (match-beginning 0))
          (mevedel-view--expand-section
           (get-text-property (point) 'mevedel-view-source)
           'agent-handle)
          (should (search-forward "Agent is running."
                                  mevedel-view--input-marker t))
          (goto-char (mevedel-view--input-start))
          (insert draft)
          (goto-char (+ (mevedel-view--input-start) 4)))
        (with-current-buffer data-buf
          (pcase-let ((`(,start . ,end)
                       (mevedel-pipeline--find-render-data-block-by-agent-id
                        agent-id)))
            (mevedel-pipeline--patch-render-data-block
             start end (plist-put (copy-sequence render-data) :calls 2))))
        (with-current-buffer view-buf
          (let ((fallbacks 0)
                (mevedel-view-agent-refresh-delay 0))
            (cl-letf (((symbol-function 'mevedel-view-rerender)
                       (lambda (&optional _buffer)
                         (cl-incf fallbacks))))
              (mevedel-view-refresh-agent-rendering view-buf agent-id))
            (should (= 0 fallbacks)))
          (should (string= draft (mevedel-view--input-text)))
          (should (= (point) (+ (mevedel-view--input-start) 4)))
          (goto-char (point-min))
          (should (search-forward "[running · 2 calls]"
                                  mevedel-view--input-marker t))
          (goto-char (match-beginning 0))
          (should-not (get-text-property (point) 'mevedel-view-collapsed))
          (should (search-forward "Agent is running."
                                  mevedel-view--input-marker t))))))

  :doc "refreshes aggregate status rows without full rerendering"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--status-refresh")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-refresh"
                       :root temporary-file-directory
                       :name "status-refresh"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "status"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-agent-status)
        (goto-char (point-min))
        (should (search-forward "1 calls" nil t))
        (setf (cdr (assoc agent-id (mevedel-session-agent-transcripts session)))
              '(:status running
                :agent-type "explorer"
                :description "status"
                :calls 2))
        (let ((fallbacks 0)
              (mevedel-view-agent-refresh-delay 0))
          (cl-letf (((symbol-function 'mevedel-view-rerender)
                     (lambda (&optional _buffer)
                       (cl-incf fallbacks))))
            (mevedel-view-refresh-agent-rendering view-buf agent-id))
          (should (= 0 fallbacks)))
        (goto-char (point-min))
        (should (search-forward "2 calls" nil t)))))

  :doc "agent status redraw preserves later queued permission prompt"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "verifier--refresh-permission")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "refresh-permission"
                       :root temporary-file-directory
                       :name "refresh-permission"))
           (session (mevedel-session-create "main" workspace))
           outcomes
           (entry (list :kind 'eval
                        :expression "(+ 20 22)"
                        :mode "live"
                        :origin agent-id
                        :session session
                        :callback (lambda (outcome)
                                    (push outcome outcomes)))))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "verifier"
                          :description "permission"
                          :calls 1))))
      (setf (mevedel-session-permission-queue session) (list entry))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-agent-status)
        (goto-char (point-min))
        (should (search-forward "Agent: verifier -- permission"
                                mevedel-view--input-marker t))
        (let ((status-bounds (mevedel-view-fragment--bounds-at
                              (match-beginning 0))))
          (should status-bounds)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'default)))
            (mevedel-permission-queue--render-head session))
          (should (string-match-p "The LLM is requesting permission to evaluate elisp"
                                  (buffer-string)))
          (should (= 1 (length (mevedel-session-permission-queue session))))
          (should-not outcomes)
          (should (overlayp mevedel-view--interaction-region-overlay))
          (should (<= (plist-get status-bounds :end)
                      (overlay-start
                       mevedel-view--interaction-region-overlay)))
          (mevedel-view--render-agent-status)
          (should (string-match-p "The LLM is requesting permission to evaluate elisp"
                                  (buffer-string)))
          (should (= 1 (length (mevedel-session-permission-queue session))))
          (should-not outcomes)
          (goto-char (point-min))
          (search-forward "Agent: verifier -- permission"
                          mevedel-view--input-marker)
          (setq status-bounds
                (mevedel-view-fragment--bounds-at (match-beginning 0)))
          (should (<= (plist-get status-bounds :end)
                      (overlay-start
                       mevedel-view--interaction-region-overlay)))))))

  :doc "falls back when data has an Agent source but no visible handle"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--missing-handle")
           (render-data (list :kind 'agent-transcript
                              :agent-id agent-id
                              :status 'running
                              :calls 1))
           bounds)
      (with-current-buffer data-buf
        (goto-char (point-max))
        (setq bounds (cons (point) nil))
        (insert "(:name \"Agent\" :args (:subagent_type \"explorer\" :description \"missing\"))\n")
        (insert "Agent is running.\n")
        (insert (mevedel-pipeline--format-render-data-block render-data))
        (setcdr bounds (point))
        (put-text-property (car bounds) (cdr bounds) 'gptel '(tool . "call-1")))
      (with-current-buffer view-buf
        (let ((fallbacks 0)
              (mevedel-view-agent-refresh-delay 0))
          (cl-letf (((symbol-function 'mevedel-view-rerender)
                     (lambda (&optional _buffer)
                       (cl-incf fallbacks))))
            (mevedel-view-refresh-agent-rendering view-buf agent-id))
          (should (= 1 fallbacks)))))))

(mevedel-deftest mevedel-view--agent-status-counts
  (:doc "ignores malformed stale FSM registry entries")
  ,test
  (test)

  :doc "malformed stale FSM entries do not break counts or aggregate rows"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--badfsm123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-bad-fsm"
                       :root temporary-file-directory
                       :name "status-bad-fsm"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "stale sidecar"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel-tools--agents-fsm
                    (list (cons agent-id (cons 'bad 'fsm)))))
      (with-current-buffer view-buf
        (should (equal (mevedel-view--agent-status-counts)
                       '(:blocked 0 :running 1)))
        (let ((rows (mevedel-view--agent-status-collect)))
          (should (= 1 (length rows)))
          (should (equal agent-id (plist-get (car rows) :agent-id))))))))

(mevedel-deftest mevedel-view-agent-status-fragment-handles
  (:doc "uses fragment-backed Agent handles for expanded aggregate status")
  ,test
  (test)

  :doc "expanded aggregate status renders Agent handles inside the status fragment"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--run123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-agent-handle"
                       :root temporary-file-directory
                       :name "status-agent-handle"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "count"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (goto-char (point-min))
        (search-forward "Agent: explorer -- count" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (should (eq 'status (get-text-property
                             (point) 'mevedel-view-fragment-namespace)))
        (should (eq 'agents (get-text-property
                             (point) 'mevedel-view-fragment-id)))
        (should (get-text-property (point) 'mevedel-view-agent-handle-p)))))

  :doc "shared activation on expanded status handles opens the Agent transcript"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-agent-handle-activate"
                       :root temporary-file-directory
                       :name "status-agent-handle-activate"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:status running
                          :agent-type "explorer"
                          :description "done"
                          :calls 1))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (setq-local mevedel--session session)
        (mevedel-view--render-status data-buf)
        (goto-char (point-min))
        (search-forward "Agent: explorer -- done" mevedel-view--input-marker)
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript-at-point)
                   (lambda (&optional _event)
                     (setq opened (get-text-property
                                   (point) 'mevedel-view-agent-id)))))
          (mevedel-view-activate-at-point))
        (should (equal agent-id opened))))))

(mevedel-deftest mevedel-view--insert-attribution
  (:doc "builds transcript attribution fragments")
  ,test
  (test)

  :doc "uses short display label and installs a deferred click target"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((s (mevedel-view--insert-attribution
                 "explorer--abcdef1234567890"))
             (pos (string-match-p "explorer--abcdef12" s)))
        (should (string-match-p "from explorer--abcdef12" s))
        (should pos)
        (should (get-text-property pos 'keymap s))
        (should (equal (get-text-property pos 'mevedel-view-agent-id s)
                       "explorer--abcdef1234567890"))
        (should (get-text-property pos 'help-echo s)))))

  :doc "completed transcript dispatches through the shared open command"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abcdef1234567890")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "attr-open"
                       :root temporary-file-directory
                       :name "attr-open"))
           (session (mevedel-session-create "main" workspace))
           (save-path (file-name-as-directory
                       (file-name-concat temporary-file-directory
                                         "mevedel-attr-open-session")))
           opened)
      (setf (mevedel-session-save-path session) save-path)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id
                        '(:path "agents/explorer--abcdef12.chat.org"
                          :status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (id) (setq opened id))))
          (mevedel-view--open-agent-transcript-or-message agent-id)
          (should (equal agent-id opened)))))))

(mevedel-deftest mevedel-view--tool-one-liner/scaffolding-prefix ()
  ,test
  (test)
  :doc "scaffolding prefix on the segment doesn't drop the tool name"
  ;; A boundary-expansion or patch can land seg-start on the
  ;; `#+begin_tool …' line (no gptel property) instead of the call
  ;; sexp.  The cleaner skips the marker so the parse still surfaces
  ;; `Bash: …' instead of bare `Tool'.
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "#+begin_tool (Bash :command \"git status\")\n"
              "(:name \"Bash\" :args (:command \"git status\"))\n"
              "\nOK\n"))
    (let ((line (mevedel-view--tool-one-liner
                 data-buf (point-min)
                 (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "Bash" line))
      (should-not (string-match-p "\\bTool\\b" line)))))


(provide 'test-mevedel-view)

;;; test-mevedel-view.el ends here
