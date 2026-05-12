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
(require 'mevedel-tool-plan)
(require 'mevedel-agents)
(require 'mevedel-hooks)
(require 'mevedel-view-history)

;; `gptel-transient'
(defvar gptel--system-message)
(defvar transient--original-buffer)
(declare-function gptel--suffix-system-message "ext:gptel-transient"
                  (&optional cancel))


;;
;;; Test helpers

(defmacro mevedel-view-test--with-buffers (&rest body)
  "Execute BODY with a data buffer and view buffer set up.
Binds `data-buf' and `view-buf' in scope.  Cleans up afterwards."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-data*"))
         (view-buf (generate-new-buffer " *test-view*")))
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
       (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(defun mevedel-view-test--insert-data (data-buf text props)
  "Insert TEXT into DATA-BUF with gptel text property PROPS.
PROPS is the value for the `gptel' property."
  (with-current-buffer data-buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when props
        (put-text-property start (point) 'gptel props)))))

(defun mevedel-view-test--capf-candidates (capf &optional prefix)
  "Return completion candidates from CAPF for PREFIX."
  (all-completions (or prefix "") (nth 2 capf)))

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
  "Block prompt submission in view send tests."
  '(:continue nil :stop-reason "blocked"))

(defvar mevedel-view-test--seen-prompt nil)

(defun mevedel-view-test--rewrite-prompt-hook (event)
  "Capture prompt EVENT and rewrite it in view send tests."
  (setq mevedel-view-test--seen-prompt (plist-get event :prompt))
  '(:updated-input "rewritten prompt"))


;;
;;; Segment extraction

(mevedel-deftest mevedel-view--extract-segments ()
  ,test
  (test)
  :doc "single user segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'user (caar segs))))))

  :doc "user + response segments"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Hello\n" nil)
    (mevedel-view-test--insert-data data-buf "Hi there\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 3 (length segs)))
        (should (eq 'response (caar segs)))
        (should (eq 'tool (caadr segs)))
        (should (eq 'response (car (caddr segs)))))))

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
             (segs (mevedel-view--extract-segments mid-start mid-end)))
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
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (equal '(response tool response) (mapcar #'car segs)))
        (should (string-prefix-p "#+begin_tool"
                                 (buffer-substring-no-properties
                                  (cadr (cadr segs))
                                  (+ (cadr (cadr segs)) 12))))
        (should (string-match-p
                 "I added the missing declaration"
                 (buffer-substring-no-properties
                  (cadr (caddr segs)) (caddr (caddr segs))))))))
  :doc "does not treat literal tool markers as tool blocks without a tool run"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "Text mentioning markers:\n#+begin_tool (Read :file_path \"/tmp/f\")\n(:name \"Read\")\n#+end_tool\n"
     'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'response (caar segs))))))
  :doc "preserves text after a stale tool run extends beyond end marker"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (block-start block-end tail-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (setq tail-start (point))
        (insert "Tail text must survive.\n")
        ;; Stale bounds can cover the block and spill into following text.
        (put-text-property (+ block-start 20) (point)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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
        (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "does not swallow legitimate user prompt between tool and response"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let (tool-start user-start response-start)
        (setq tool-start (point))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call_1"))
        (setq user-start (point))
        (insert "please explain that output\n")
        (setq response-start (point))
        (insert "sure, here is the explanation\n")
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segments (mevedel-view--extract-segments
                         (point-min) (point-max))))
          (should (equal '(tool user response)
                         (mapcar #'car segments)))))))

  :doc "ignore segment"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "thinking...\n" 'ignore)
    (with-current-buffer data-buf
      (let ((segs (mevedel-view--extract-segments (point-min) (point-max))))
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

  :doc "real user prompt after response is not absorbed before reasoning"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "First answer.\n" 'response)
    (mevedel-view-test--insert-data
     data-buf "\n\nSecond prompt.\n\n#+begin_reasoning\n" nil)
    (mevedel-view-test--insert-data data-buf "thinking\n" 'ignore)
    (with-current-buffer data-buf
      (let* ((segments (mevedel-view--extract-segments (point-min) (point-max)))
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
      (let* ((segments (mevedel-view--extract-segments (point-min) (point-max)))
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
        (should (string-match-p "3 lines" summary)))))

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
                                summary)))))

  :doc "fallback on unparseable content"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "not a valid sexp" '(tool . "call_3"))
    (with-current-buffer data-buf
      (let ((summary (mevedel-view--tool-one-liner data-buf (point-min) (point-max))))
        (should (stringp summary))
        (should (> (length summary) 0))))))

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

  :doc "normalizes gptel-org converted source blocks in responses"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "Here is code:\n#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n"
     'response)
    (with-current-buffer data-buf
      (mevedel-view--render-response (point-min) (point-max)))
    (with-current-buffer view-buf
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p "```emacs-lisp" text))
        (should (string-match-p "(message \"hi\")" text))
        (should-not (string-match-p "#\\+begin_src" text))
        (should-not (string-match-p "#\\+end_src" text)))))

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

  :doc "renders inline slash skills as compact invocation with collapsed prompt"
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
                  "/emacs-context-snapshot\nSay hi!")))
        (put-text-property start (point) 'gptel 'ignore)))
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((text (buffer-substring-no-properties
                   (point-min) mevedel-view--input-marker)))
        (should (string-match-p
                 "/emacs-context-snapshot\nSay hi!"
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
  :doc "creates and removes spinner overlay"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Working...")
      (should mevedel-view--spinner-overlay)
      (should (overlay-buffer mevedel-view--spinner-overlay))
      (let ((text (buffer-substring-no-properties
                   (overlay-start mevedel-view--spinner-overlay)
                   (overlay-end mevedel-view--spinner-overlay))))
        (should (string-match-p "Working" text)))
      (mevedel-view--stop-spinner)
      (should-not mevedel-view--spinner-overlay)))

  :doc "update replaces spinner text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--update-spinner "Calling Read...")
      (let ((text (buffer-substring-no-properties
                   (overlay-start mevedel-view--spinner-overlay)
                   (overlay-end mevedel-view--spinner-overlay))))
        (should (string-match-p "Calling Read" text))
        (should-not (string-match-p "Thinking" text)))
      (mevedel-view--stop-spinner)))

  :doc "ASCII fallback frames can be selected"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((mevedel-view-spinner-animate nil)
            (mevedel-view-spinner-frames mevedel-view-spinner-ascii-frames)
            (mevedel-view--spinner-frame-index 0))
        (mevedel-view--start-spinner "Working...")
        (let ((text (buffer-substring-no-properties
                     (overlay-start mevedel-view--spinner-overlay)
                     (overlay-end mevedel-view--spinner-overlay))))
          (should (string-prefix-p "- Working" text)))
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

  :doc "pre-tool render replaces overlay spinner with animated pending line"
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
        (should-not mevedel-view--spinner-overlay)
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-inline-spinner-frame t))
        (should (text-property-any
                 (point-min) (point-max)
                 'mevedel-view-pending-tool-live t))
        (let ((text (buffer-substring-no-properties
                     (point-min) (point-max))))
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
      (should-not mevedel-view--spinner-overlay)
      (should-not (string-match-p "Calling Agent"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))

  :doc "stop tolerates a detached overlay without crashing"
  ;; A rerender that wipes the spinner's anchor region leaves the
  ;; overlay's `overlay-start' / `overlay-end' returning nil; without
  ;; the guard, `delete-region' would signal
  ;; `wrong-type-argument: integer-or-marker-p, nil'.
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (let ((ov mevedel-view--spinner-overlay))
        ;; Detach the overlay manually -- simulates a rerender that
        ;; wiped the anchor region.
        (delete-overlay ov)
        ;; Re-install on the variable so stop-spinner sees a detached
        ;; overlay (one whose `overlay-start' returns nil).
        (setq mevedel-view--spinner-overlay ov))
      (should (progn (mevedel-view--stop-spinner) t))
      (should-not mevedel-view--spinner-overlay)))

  :doc "stop deletes overlayless spinner text"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view--start-spinner "Thinking...")
      (delete-overlay mevedel-view--spinner-overlay)
      (setq mevedel-view--spinner-overlay nil)
      (mevedel-view--stop-spinner)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
        (should-not (string-match-p "Thinking" text)))))

  :doc "stop does not delete non-spinner text if an overlay went stale"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((start nil)
            (inhibit-read-only t))
        (goto-char mevedel-view--input-marker)
        (setq start (point))
        (insert "Assistant\n› Calling Read...\n")
        (let ((ov (make-overlay start (point) nil t)))
          (overlay-put ov 'mevedel-view-spinner t)
          (setq mevedel-view--spinner-overlay ov)))
      (mevedel-view--stop-spinner)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Assistant" text))
        (should (string-match-p "Calling Read" text)))
      (should-not mevedel-view--spinner-overlay))))


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
      (should (eq mevedel--view-buffer view-buf))))

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
                   (error "view buffer requested save filename")))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (setq prompted t)
                   (error "view buffer requested save confirmation"))))
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


;;
;;; gptel transient proxy

(mevedel-deftest mevedel-view--gptel-target-buffer ()
  ,test
  (test)
  :doc "view buffer gptel operations target the data buffer"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (should (eq (mevedel-view--gptel-target-buffer) data-buf))))

  :doc "data buffer remains its own gptel target"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (should (eq (mevedel-view--gptel-target-buffer) data-buf))))

  :doc "transient original buffer supplies the target from popup buffers"
  (mevedel-view-test--with-buffers
    (let ((transient--original-buffer data-buf))
      (with-temp-buffer
        (should (eq (mevedel-view--gptel-target-buffer) data-buf))))))

(mevedel-deftest mevedel-view--gptel-target-advice ()
  ,test
  (test)
  :doc "advice reads buffer-local gptel state from the data buffer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local gptel--system-message "data prompt"))
    (with-current-buffer view-buf
      (setq-local gptel--system-message "view prompt")
      (should
       (equal (mevedel-view--gptel-target-advice
               (lambda ()
                 (list (current-buffer) gptel--system-message)))
              (list data-buf "data prompt")))))

  :doc "advice uses transient original buffer for nested gptel menus"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local gptel--system-message "data prompt"))
    (let ((transient--original-buffer data-buf))
      (with-temp-buffer
        (setq-local gptel--system-message "temporary prompt")
        (should
         (equal (mevedel-view--gptel-target-advice
                 (lambda ()
                   (list (current-buffer) gptel--system-message)))
                (list data-buf "data prompt")))))))

  :doc "raw data-buffer invocations do not schedule view restoration"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local gptel--system-message "data prompt")
      (should
       (equal (mevedel-view--gptel-target-advice
               (lambda ()
                 (list (current-buffer) gptel--system-message)))
              (list data-buf "data prompt")))
    (should-not mevedel-view--gptel-return-view-buffer)))

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
    (let ((position (copy-marker 1 nil)))
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

(defun mevedel-view-test--interactive-command (system-message)
  "Return SYSTEM-MESSAGE and current buffer for advice tests."
  (interactive (list gptel--system-message))
  (list system-message (current-buffer)))

(mevedel-deftest mevedel-view--gptel-target-interactive ()
  ,test
  (test)
  :doc "interactive argument collection runs in the data buffer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (setq-local gptel--system-message "data prompt"))
    (with-current-buffer view-buf
      (setq-local gptel--system-message "view prompt"))
    (unwind-protect
        (progn
          (advice-add 'mevedel-view-test--interactive-command
                      :around #'mevedel-view--gptel-target-advice)
          (with-current-buffer view-buf
            (should
             (equal (call-interactively
                     #'mevedel-view-test--interactive-command)
                    (list "data prompt" data-buf)))))
      (advice-remove 'mevedel-view-test--interactive-command
                     #'mevedel-view--gptel-target-advice))))

(mevedel-deftest mevedel-view--gptel-system-message-suffix ()
  ,test
  (test)
  :doc "system-message suffix guard sees function-valued data prompt"
  (skip-unless (require 'gptel-transient nil t))
  (mevedel-view-test--with-buffers
    (let ((guard-count 0)
          (menu-count 0))
      (with-current-buffer data-buf
        (setq-local gptel--system-message (lambda () "generated prompt")))
      (with-current-buffer view-buf
        (setq-local gptel--system-message nil))
      (unwind-protect
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (cl-incf guard-count)
                       nil))
                    ((symbol-function 'gptel-menu)
                     (lambda ()
                       (interactive)
                       (cl-incf menu-count))))
            (advice-add 'gptel--suffix-system-message
                        :around #'mevedel-view--gptel-target-advice)
            (with-current-buffer view-buf
              (call-interactively #'gptel--suffix-system-message))
            (should (= guard-count 1))
            (should (= menu-count 1)))
        (advice-remove 'gptel--suffix-system-message
                       #'mevedel-view--gptel-target-advice)))))

(mevedel-deftest mevedel-view--gptel-transient-advice-install ()
  ,test
  (test)
  :doc "deferred installer is a no-op after uninstall disables the proxy"
  (let ((calls 0)
        (mevedel-view--gptel-transient-advice-installed nil))
    (cl-letf (((symbol-function 'mevedel-view--install-gptel-transient-advice)
               (lambda () (cl-incf calls))))
      (mevedel-view--install-gptel-transient-advice-if-enabled)
      (should (= calls 0))
      (let ((mevedel-view--gptel-transient-advice-installed t))
        (mevedel-view--install-gptel-transient-advice-if-enabled))
      (should (= calls 1))))

  :doc "uninstall disables future deferred installs"
  (let ((mevedel-view--gptel-transient-advice-installed t))
    (cl-letf (((symbol-function 'mevedel-view--uninstall-gptel-transient-advice)
               #'ignore))
      (mevedel-view-uninstall-gptel-menu-advice))
    (should-not mevedel-view--gptel-transient-advice-installed)))

(mevedel-deftest mevedel-view--gptel-return-to-view ()
  ,test
  (test)
  :doc "transient exit restores the view when gptel left data selected"
  (mevedel-view-test--with-buffers
    (let ((window (selected-window)))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (mevedel-view--gptel-schedule-return-to-view view-buf data-buf)
            (should (eq mevedel-view--gptel-return-window window))
            (set-window-buffer window data-buf)
            (should (eq (window-buffer window) data-buf))
            (mevedel-view--gptel-return-to-view)
            (should (eq (window-buffer window) view-buf))
            (should-not mevedel-view--gptel-return-view-buffer)
            (should-not (memq #'mevedel-view--gptel-return-to-view
                              transient-post-exit-hook)))
        (when (window-live-p window)
          (set-window-buffer window view-buf)))))

  :doc "prompt editing keeps restoration pending until data is displayed"
  (mevedel-view-test--with-buffers
    (let ((window (selected-window))
          (prompt-buffer (get-buffer-create "*gptel-prompt*")))
      (unwind-protect
          (progn
            (set-window-buffer window view-buf)
            (mevedel-view--gptel-schedule-return-to-view view-buf data-buf)
            (with-current-buffer prompt-buffer
              (mevedel-view--gptel-return-to-view))
            (should (eq mevedel-view--gptel-return-view-buffer view-buf))
            (should (memq #'mevedel-view--gptel-return-to-view
                          transient-post-exit-hook))
            (set-window-buffer window data-buf)
            (mevedel-view--gptel-return-to-view)
            (should (eq (window-buffer window) view-buf))
            (should-not mevedel-view--gptel-return-view-buffer))
        (when (buffer-live-p prompt-buffer)
          (kill-buffer prompt-buffer))
        (when (window-live-p window)
          (set-window-buffer window view-buf))))))

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


;;
;;; Input forwarding

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
  :doc "view slash completion refreshes after skill saves"
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
            (insert "/")
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
	      (delete-directory root t))))


;;
;;; Full re-render

(mevedel-deftest mevedel-view--fontify-response ()
  ,test
  (test)
  :doc "does not install Org agenda menus while fontifying response text"
  (require 'org)
  (let ((org-agenda-file-menu-enabled t)
        menu-called)
    (cl-letf (((symbol-function 'org-install-agenda-files-menu)
               (lambda ()
                 (setq menu-called t)
                 (error "menu setup should not run"))))
      (let ((text (mevedel-view--fontify-response
                   "I’ll inspect =mevedel-review.el= now.")))
        (should (string-match-p "mevedel-review\\.el" text))
        (should-not menu-called)))))

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
        (should (string-match-p "Read: /tmp/a.png" text))
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

  :doc "skips leading compaction summary in rotated segment"
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

  :doc "restores spinner overlay for preserved in-flight live tail"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data data-buf "*** Prompt\n" nil)
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker nil))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-max) nil)))
      (mevedel-view--start-spinner "Thinking...")
      (mevedel-view--full-rerender)
      (should mevedel-view--spinner-overlay)
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
  (:doc "renders and rebuilds composite interaction-zone overlays")
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
      (should (overlayp mevedel-view--interaction-separator-overlay))
      (should (string-suffix-p
               "\n\n"
               (overlay-get mevedel-view--interaction-separator-overlay
                            'before-string)))
      (should (overlayp mevedel-view--interaction-materialized-overlay))
      (should (string-match-p "plan" (buffer-string)))
      (should (string-match-p "permission" (buffer-string)))
      (maphash
       (lambda (_id overlay)
         (should (< (overlay-start overlay) (overlay-end overlay)))
         (should (overlay-get overlay 'mevedel-view-interaction-entry))
         (should (overlay-get overlay 'mevedel-view-interaction-activate))
         (should (overlay-get overlay 'keymap))
         (should-not (overlay-get overlay 'before-string))
         (should (get-text-property
                  (overlay-start overlay)
                  'mevedel-view-interaction-overlay)))
       mevedel-view--interaction-overlays)))

  :doc "does not focus interaction prompt while a live window is drafting"
  (mevedel-view-test--with-buffers
    (switch-to-buffer view-buf)
    (delete-other-windows)
    (with-current-buffer view-buf
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (goto-char (+ (mevedel-view--input-start) 2))
      ;; Simulate buffer point drifting away from the selected window point.
      ;; Prompt focus must respect the live cursor, not this stale value.
      (save-excursion
        (goto-char (point-min))))
    (with-current-buffer view-buf
      (mevedel-view--interaction-register
       (list :kind 'permission :id 'permission :count 1
             :body "\npermission\n" :keymap (make-sparse-keymap)
             :help-echo "Permission" :entry 'permission-entry
             :activate #'ignore))
      (should (= (window-point (selected-window))
                 (+ (mevedel-view--input-start) 2))))
    (delete-other-windows))

  :doc "incremental history render stays above materialized interaction UI"
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

(mevedel-deftest mevedel-view--skip-leading-properties-drawer ()
  ,test
  (test)
  :doc "advances past a well-formed :PROPERTIES: drawer at POS"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_MODEL: x\n:END:\nhello\n")
    (let ((after (mevedel-view--skip-leading-properties-drawer (point-min))))
      (should (> after (point-min)))
      (should (string= "hello\n" (buffer-substring-no-properties
                                  after (point-max))))))
  :doc "returns POS unchanged when no drawer is present"
  (with-temp-buffer
    (insert "no drawer here\n")
    (should (= (point-min)
               (mevedel-view--skip-leading-properties-drawer (point-min)))))
  :doc "returns POS unchanged when drawer is malformed (no :END:)"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_MODEL: x\nstuff\n")
    (should (= (point-min)
               (mevedel-view--skip-leading-properties-drawer (point-min))))))

(mevedel-deftest mevedel-view--skip-leading-summary-block ()
  ,test
  (test)
  :doc "advances past a leading compaction summary block"
  (with-temp-buffer
    (insert "#+begin_summary mevedel-role=compaction-summary\nsummary\n#+end_summary\nlive\n")
    (let ((after (mevedel-view--skip-leading-summary-block (point-min))))
      (should (> after (point-min)))
      (should (string= "live\n" (buffer-substring-no-properties
                                 after (point-max))))))
  :doc "returns POS unchanged when no summary starts there"
  (with-temp-buffer
    (insert "live\n")
    (should (= (point-min)
               (mevedel-view--skip-leading-summary-block (point-min))))))


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
          (should (string-match-p "full content here" text)))))))

(mevedel-deftest mevedel-view-toggle-section/renderer-vtype ()
  ,test
  (test)
  :doc "agent handles expand and collapse through their renderer"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "(:name \"Agent\" :args (:subagent_type \"explorer\"))\n\nraw launch payload\n"))
    (with-current-buffer view-buf
      (let* ((source (cons 1 (with-current-buffer data-buf (point-max))))
             (rendering '(:header "Agent: explorer -- Find calls"
                          :body "rendered agent body\n"
                          :body-mode text-mode
                          :vtype agent-handle)))
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (let ((start (point)))
                (insert "› Agent: explorer -- Find calls\nrendered agent body\n")
                (add-text-properties
                 start (point)
                 `(font-lock-face mevedel-view-tool-summary
                   mevedel-view-type agent-handle
                   mevedel-view-collapsed nil
                   mevedel-view-source ,source
                   read-only t
                   keymap ,mevedel-view--display-map
                   front-sticky (read-only keymap)
                   rear-nonsticky (read-only keymap))))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (cl-letf (((symbol-function 'mevedel-view--segment-rendering)
                   (lambda (buf start end)
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
                        'agent-handle))
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

(mevedel-deftest mevedel-tool-ui--render-agent-body
  (:doc "selects the correct Agent expanded body for foreground and background rows")
  ,test
  (test)

  :doc "completed background Agent rows render saved activity instead of launch text"
  (mevedel-view-test--with-buffers
    (let* ((mevedel-view-agent-activity-max 3)
           (args '(:subagent_type "explorer" :description "Task"))
           (launch "Agent launched in background: explorer")
           (rd '(:kind agent-transcript
                 :agent-id "explorer--5cc58945"
                 :background t
                 :status completed
                 :activity ((:type message :from "main")
                            (:type waiting)
                            (:type tool-start :tool-name "SendMessage")
                            (:type tool-finish :tool-name "SendMessage")
                            (:type waiting)
                            (:type tool-start :tool-name "Read"))))
           (rendering (mevedel-tool-ui--render-agent
                       "Agent" args launch rd)))
      (should (string-match-p "(6 lines)" (plist-get rendering :header)))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (set-marker-insertion-type mevedel-view--input-marker t)
          (unwind-protect
              (mevedel-view--render-expanded-body rendering (cons 1 1))
            (set-marker-insertion-type mevedel-view--input-marker nil)))
        (let ((text (buffer-substring-no-properties
                     (point-min) mevedel-view--input-marker)))
          (should (string-match-p "✉ message from main" text))
          (should (string-match-p "… waiting" text))
          (should (string-match-p "-> SendMessage" text))
          (should (string-match-p "✓ SendMessage done" text))
          (should (string-match-p "-> Read" text))
          (should-not (string-match-p "Agent launched in background" text))))))

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

  :doc "Agent handle transcript click target is limited to the attribution id"
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
          (goto-char start)
          (search-forward "Agent: explorer")
          (goto-char (match-beginning 0))
          (should (eq (get-text-property (point) 'keymap)
                      mevedel-view--agent-handle-map))
          (should-not
           (lookup-key (get-text-property (point) 'keymap) [mouse-1]))
          (should-not
           (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
          (goto-char start)
          (search-forward "explorer--abcdef12")
          (goto-char (match-beginning 0))
          (should (equal agent-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should-not (eq (get-text-property (point) 'keymap)
                          mevedel-view--agent-handle-map))
          (should
           (lookup-key (get-text-property (point) 'keymap) [mouse-1]))))))

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
        (should (string-match-p "Third line" text))))))

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
  :doc "rejects missing :header"
  (should-not (mevedel-view--rendering-plist-p '(:body "b")))
  :doc "rejects non-string :header"
  (should-not (mevedel-view--rendering-plist-p '(:header 42)))
  :doc "rejects non-string :body"
  (should-not (mevedel-view--rendering-plist-p '(:header "h" :body 42)))
  :doc "rejects non-symbol :body-mode"
  (should-not (mevedel-view--rendering-plist-p
               '(:header "h" :body-mode "not-a-symbol"))))


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
                            (error "oops"))))
         (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (should (null (mevedel-view--invoke-renderer
                     tool '(:kind diff) nil "ok")))
      (should warnings)
      (should (eq 'mevedel (caar warnings)))
      (should (string-match-p "failed" (cadar warnings))))))


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
             (segs (mevedel-view--extract-segments mid-start mid-end))
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
                     data-buf (point-min) (point-max)))))))


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
    (let ((render-count 0))
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

  :doc "tool hooks do not return rendered agent-status strings to gptel"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (setq mevedel-view--in-flight-turn-start
            (copy-marker mevedel-view--input-marker))
      (setq mevedel-view--data-turn-start
            (with-current-buffer data-buf (copy-marker (point-min)))))
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
          '(:id "call-1" :name "Read" :args (:file_path "a")))))))

  :doc "Agent pre-tool hook does not add a duplicate pending Calling Agent line"
  (mevedel-view-test--with-buffers
    (let ((render-count 0))
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

  :doc "cleanup removes older live tails tagged only on the spinner frame"
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
      (should-not (string-match-p "Calling Agent"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))

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

  :doc "incremental render owns spinner text before pending tool replacement"
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
      (should-not mevedel-view--spinner-overlay)
      (mevedel-view--stop-spinner)
      (let ((text (buffer-substring-no-properties
                   (point-min) (point-max))))
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
        (should (string-search "… waiting" text))
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

(defmacro mevedel-view-test--with-fork-skill (skill-form &rest body)
  "Wire data-buf with a session containing SKILL-FORM, then run BODY.
Binds `data-buf', `view-buf', and `session' in scope.  The skill is
attached via `mevedel-session-skills' so `mevedel-session-get-skill'
finds it during slash dispatch."
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
  :doc "fork slash blocks input, captures the callback, and inserts the result"
  ;; Drive `mevedel-view-send' for a /myfork dispatch with a fork
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
          (insert "/myfork run a thing")
          (mevedel-view-send)

          ;; The view armed the in-flight turn marker and spinner.
          (should (markerp mevedel-view--in-flight-turn-start))
          (should (marker-position mevedel-view--in-flight-turn-start))
          (should mevedel-view--spinner-overlay)

          ;; The user-message display text appeared in the view above
          ;; the input region.
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "/myfork run a thing" text))))
        (with-current-buffer data-buf
          (should mevedel--current-request))

        ;; mevedel-skills-invoke was called with the right shape.
        (should captured-args)
        (should (equal "myfork"
                       (mevedel-skill-name (plist-get captured-args :skill))))
        (should (equal "run a thing" (plist-get captured-args :args)))
        (should (eq 'user-slash
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
          (insert "/myfork run")
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
        ;; Spinner overlay was removed by `--stop-spinner'.
        (with-current-buffer view-buf
          (should-not mevedel-view--spinner-overlay)))))))

(mevedel-deftest mevedel-view-send/skill-inline ()
  ,test
  (test)
  :doc "inline slash skill forwards expanded body with render-data side channel"
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
          (insert "/myskill hello")
          (mevedel-view-send)
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p "/myskill\nhello" text))
            (should-not (string-match-p "Expanded hello" text))))
        (should send-called)
        (with-current-buffer data-buf
	          (let ((text (buffer-string)))
	            (should (string-match-p "Expanded hello" text))
	            (should (string-search "<!-- mevedel-render-data -->" text))
	            (should (equal
	                     (mevedel-pipeline--strip-render-data-blocks text)
	                     "\n\n*** Expanded hello\n"))))))))

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
                         (mevedel-view--interaction-count-label))))
      (should (equal "follow up"
                     (plist-get
                      (car (mevedel-session-queued-user-messages session))
                      :input)))
      (with-current-buffer data-buf
        (should (string-empty-p (buffer-string)))))))

  :doc "queued message UI shows edit and clear key hints"
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
        (should (string-match-p "C-c C-e edit latest; C-c C-q clear"
                                (buffer-string))))))

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
      (goto-char (mevedel-view--input-start))
      (insert "draft")
      (goto-char (+ (mevedel-view--input-start) 2))
      (mevedel-view--start-spinner "Thinking...")
      (should (= (point) (+ (mevedel-view--input-start) 2)))
      (mevedel-view--update-spinner "Calling Read...")
      (should (= (point) (+ (mevedel-view--input-start) 2)))
      (mevedel-view--stop-spinner)
      (should (= (point) (+ (mevedel-view--input-start) 2)))
      (should (string= "draft" (mevedel-view--input-text)))))

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

  :doc "queued messages drain one at a time in FIFO order"
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
        (mevedel-view--drain-one-queued-user-message data-buf)
        (should (= 1 sent))
        (should (equal '("second")
                       (mapcar (lambda (entry) (plist-get entry :input))
                               (mevedel-session-queued-user-messages
                                session))))
        (with-current-buffer data-buf
          (should (string-match-p "first" (buffer-string)))
          (should-not (string-match-p "second" (buffer-string)))))))

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
        (mevedel-view--drain-one-queued-user-message data-buf)
        (should-not sent)
        (should (mevedel-session-queued-user-messages session)))))

  :doc "editing the latest queued message removes it from auto-submit"
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
        (should (string= "second" (mevedel-view--input-text)))
        (should (equal '("first")
                       (mapcar (lambda (entry) (plist-get entry :input))
                               (mevedel-session-queued-user-messages
                                session)))))
      (cl-letf (((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-one-queued-user-message data-buf)
        (should-not sent))))

  :doc "queued drain does not clear composer if a hook returns after queued edit"
  (mevedel-view-test--with-buffers
    (let* ((ws (mevedel-workspace--create
                :type 'test :id "vq-edit-async" :root "/tmp/vq" :name "vq"
                :file-cache (mevedel-file-cache--create
                             :table (make-hash-table :test #'equal)
                             :order nil :total-bytes 0)))
           (session (mevedel-session-create "main" ws))
           hook-callback
           sent)
      (with-current-buffer data-buf
        (setq-local mevedel--session session)
        (setq-local mevedel--workspace ws))
      (setf (mevedel-session-queued-user-messages session)
            (list (list :input "first" :display-text "first")
                  (list :input "second" :display-text "second")))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _event-plist callback &rest _)
                   (setq hook-callback callback)))
                ((symbol-function 'mevedel-hooks-event-plist)
                 (lambda (&rest _) nil))
                ((symbol-function 'mevedel-hooks-additional-context-string)
                 (lambda (&rest _) nil))
                ((symbol-function 'gptel-send)
                 (lambda (&rest _) (setq sent t))))
        (mevedel-view--drain-one-queued-user-message data-buf)
        (should hook-callback)
        (with-current-buffer view-buf
          (should mevedel-view--prompt-hook-pending)
          (mevedel-view-edit-last-queued-message)
          (should (string= "second" (mevedel-view--input-text))))
        (funcall hook-callback nil)
        (should-not sent)
        (with-current-buffer view-buf
          (should-not mevedel-view--prompt-hook-pending)
          (should (string= "second" (mevedel-view--input-text))))
        (should (equal '("first")
                       (mapcar (lambda (entry) (plist-get entry :input))
                               (mevedel-session-queued-user-messages
                                session))))
        (with-current-buffer data-buf
          (should (string-empty-p (buffer-string))))))))

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
	      (should (string-empty-p (buffer-string)))))))
      (delete-directory root t))

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
	  (insert "/myskill blocked")
	  (mevedel-view-send)
	          (should-not send-called)
	          (should-not (mevedel-view-history--entries)))
	        (with-current-buffer data-buf
	          (should (string-empty-p (buffer-string)))
	          (should-not (bound-and-true-p
	                       mevedel-skills--pending-request-context))))))

  :doc "inline slash hooks see expanded body and can rewrite it"
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
          (insert "/myskill hello")
          (mevedel-view-send))
	        (should send-called)
	        (should (string-match-p "Expanded hello"
	                                mevedel-view-test--seen-prompt))
	        (should-not (string-search "<!-- mevedel-render-data -->"
	                                   mevedel-view-test--seen-prompt))
	        (with-current-buffer data-buf
	          (let ((text (buffer-string)))
	            (should (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "Expanded hello" text)))))))

  :doc "rewritten fork slash prompt sends as normal prompt without invoking skill"
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
          (insert "/myfork original")
          (mevedel-view-send))
        (should send-called)
        (should-not invoke-called)
        (with-current-buffer data-buf
          (let ((text (buffer-string)))
            (should (string-match-p "rewritten prompt" text))
            (should-not (string-match-p "/myfork original" text)))))))

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
  :doc "normalizes converted org source block markers"
  (mevedel-view-test--with-buffers
    (mevedel-view-test--insert-data
     data-buf
     "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n"
     'response)
    (let ((summary (mevedel-view--response-summary
                    data-buf
                    (with-current-buffer data-buf (point-min))
                    (with-current-buffer data-buf (point-max)))))
      (should (string-match-p "```emacs-lisp" summary))
      (should-not (string-match-p "#\\+begin_src" summary)))))

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
     "Real user prompt here.\n\n<hook-context>\nModel-only context.\n</hook-context>\n"
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
        (should (string-match-p "Model-only context" text))))))

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
        (should (string-match-p "result" text))
        (should (string-match-p "Assistant\n" text))
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

  :doc "running transcript attribution is clickable and reports still-running"
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
          (should (string-match-p "still running" message-text))
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

  :doc "read-only attach does not open running transcripts through attribution"
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
        (should (string-match-p "still running" message-text))))))

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
                    #'mevedel-view-open-agent-transcript-at-point))
        (cl-letf (((symbol-function
                    'mevedel-view--open-agent-transcript-or-message)
                   (lambda (id &rest _) (setq opened id))))
          (mevedel-view-open-agent-transcript-at-point)
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
  (:doc "dispatches running handles to activity toggle and terminal handles to transcript open")
  ,test
  (test)

  :doc "running handle toggles activity expansion instead of opening transcript"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "handle"
                       :root temporary-file-directory
                       :name "handle"))
           (session (mevedel-session-create "main" workspace))
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
        (should (> mevedel-view-agent-activity-max 0))
        (mevedel-view--set-agent-expanded agent-id nil)
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t)))
                  ((symbol-function 'mevedel-view--full-rerender)
                   (lambda () nil)))
          (mevedel-view-agent-handle-activate)
          (should-not opened)
          (should (plist-get (mevedel-view--agent-activity-state agent-id)
                             :expanded))))))

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
          (should (equal agent-id opened)))))))

(mevedel-deftest mevedel-view--agent-activity-state
  (:doc "tracks running activity expansion across block/unblock cycles")
  ,test
  (test)

  :doc "blocked auto-expands and unblock restores the pre-block state"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--abc123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "activity-state"
                       :root temporary-file-directory
                       :name "activity-state"))
           (session (mevedel-session-create "main" workspace)))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (mevedel-view--set-agent-expanded agent-id nil)
        (setf (mevedel-session-permission-queue session)
              (list (list :origin agent-id)))
        (mevedel-view--agent-normalize-expansion-state agent-id 'running)
        (should (plist-get (mevedel-view--agent-activity-state agent-id)
                           :expanded))
        (setf (mevedel-session-permission-queue session) nil)
        (mevedel-view--agent-normalize-expansion-state agent-id 'running)
        (should-not (plist-get (mevedel-view--agent-activity-state agent-id)
                               :expanded)))))

  :doc "terminal transition clears stored expansion state"
  (mevedel-view-test--with-buffers
    (let ((agent-id "explorer--abc123"))
      (with-current-buffer view-buf
        (mevedel-view--set-agent-expanded agent-id t)
        (mevedel-view--agent-normalize-expansion-state agent-id 'completed)
        (should-not (gethash agent-id mevedel-view--agent-activity-expanded))))))

(mevedel-deftest mevedel-view--agent-transcript-window
  (:doc "manages the singleton transcript side window")
  ,test
  (test)

  :doc "new transcript reuses the prior singleton and manual kill clears parent"
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
          (with-current-buffer parent
            (mevedel-view-mode)
            (setq-local mevedel-view--agent-transcript-window
                        (selected-window))
            (set-window-buffer (selected-window) old-view)
            (setq reused-window (selected-window))
            (mevedel-view--display-agent-transcript-view new-view)
            (should (eq reused-window mevedel-view--agent-transcript-window))
            (should (eq (window-buffer reused-window) new-view))
            (should (buffer-live-p old-view))
            (should (window-live-p mevedel-view--agent-transcript-window)))
          (kill-buffer new-view)
          (with-current-buffer parent
            (should-not mevedel-view--agent-transcript-window)))
      (when (buffer-live-p new-view) (kill-buffer new-view))
      (when (buffer-live-p new-data) (kill-buffer new-data))
      (when (buffer-live-p old-view) (kill-buffer old-view))
      (when (buffer-live-p old-data) (kill-buffer old-data))
      (when (buffer-live-p parent) (kill-buffer parent))))

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
                       (lambda (&rest _) (error "display failed")))
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
                           :calls 1)))))
    (should (string-suffix-p "\n\n" text)))

  :doc "aggregate status toggle is attached to the suffix button only"
  (let* ((text (mevedel-view--agent-status-string
                (list (list :agent-id "explorer--abc"
                            :status 'running
                            :description "count"
                            :calls 1))))
         (button-pos (string-match-p (regexp-quote "[+]") text)))
    (should-not (lookup-key mevedel-view-mode-map (kbd "C-c C-a")))
    (should button-pos)
    (should (eq (lookup-key (get-text-property button-pos 'keymap text)
                            (kbd "RET"))
                #'mevedel-view-agent-status-toggle))
    (should (get-text-property button-pos 'follow-link text))
    (should-not (get-text-property (max 0 (1- button-pos))
                                   'keymap text)))

  :doc "status fallback is materialized as an Agent handle"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let ((agent-id "explorer--materialized"))
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
          (should (overlayp mevedel-view--agent-status-overlay))))))

  :doc "status fallback handles survive repeated refreshes"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--refresh123")
           (fake-fsm (cons 'refresh 'fsm))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "count"
                 :transcript-status 'running
                 :call-count 1))
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

  :doc "expanding one status fallback handle does not expand siblings"
  (mevedel-view-test--with-buffers
    (let* ((first-id "explorer--first123")
           (second-id "explorer--second456")
           (first-fsm (cons 'first 'fsm))
           (second-fsm (cons 'second 'fsm))
           (first-inv (mevedel-agent-invocation--create
                       :agent-id first-id
                       :description "count defvars"
                       :transcript-status 'running
                       :call-count 2
                       :activity '((:type waiting))))
           (second-inv (mevedel-agent-invocation--create
                        :agent-id second-id
                        :description "count defcustoms"
                        :transcript-status 'running
                        :call-count 1
                        :activity '((:type waiting))))
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
          (mevedel-view--set-agent-expanded first-id nil)
          (mevedel-view--set-agent-expanded second-id nil)
          (mevedel-view--render-agent-status)
          (when (overlayp mevedel-view--agent-status-overlay)
            (let ((inhibit-read-only t))
              (put-text-property
               (overlay-start mevedel-view--agent-status-overlay)
               (overlay-end mevedel-view--agent-status-overlay)
               'mevedel-view-source
               (cons 1 1))))
          (goto-char (point-min))
          (search-forward "Agent: explorer -- count defvars"
                          mevedel-view--input-marker)
          (goto-char (match-beginning 0))
          (should (equal first-id
                         (get-text-property
                          (point) 'mevedel-view-agent-id)))
          (should (eq 'running
                      (get-text-property
                       (point) 'mevedel-view-agent-status)))
          (should (get-text-property (point) 'mevedel-view-source))
          (should-not (plist-get (mevedel-view--agent-activity-state first-id)
                                 :expanded))
          (mevedel-view-toggle-section)
          (should (plist-get (mevedel-view--agent-activity-state first-id)
                             :expanded))
          (should-not (plist-get
                       (mevedel-view--agent-activity-state second-id)
                       :expanded))
          (let ((text (buffer-substring-no-properties
                       (point-min) mevedel-view--input-marker)))
            (should (string-match-p
                     "Agent: explorer -- count defvars[^\n]*\n… waiting"
                     text))
            (should-not
             (string-match-p
              "Agent: explorer -- count defcustoms[^\n]*\n… waiting"
              text)))))))

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

  :doc "terminal live invocation overrides stale running sidecar in aggregate"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--race123")
           (fake-fsm (cons 'fake 'fsm))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :transcript-status 'completed
                 :call-count 5))
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
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "running\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t)))
        (cl-letf (((symbol-function 'mevedel-tools--agent-invocation-at)
                   (lambda (fsm)
                     (and (eq fsm fake-fsm) inv))))
          (let ((rows (mevedel-view--agent-status-collect)))
            (should (null rows)))))))

  :doc "live agent without a visible handle still appears in aggregate status"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--hidden123")
           (fake-fsm (cons 'hidden 'fsm))
           (inv (mevedel-agent-invocation--create
                 :agent-id agent-id
                 :description "hidden task"
                 :transcript-status 'running
                 :call-count 3))
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

(mevedel-deftest mevedel-view-agent-status-activate-row
  (:doc "reveals aggregate rows without opening transcripts")
  ,test
  (test)

  :doc "running row reveals handle and expands activity"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--run123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-reveal"
                       :root temporary-file-directory
                       :name "status-reveal"))
           (session (mevedel-session-create "main" workspace)))
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status running))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "handle\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t))
          (insert (mevedel-view--agent-status-row-string
                   (list :agent-id agent-id :status 'running))))
        (goto-char (point-min))
        (search-forward "explorer--run123")
        (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                   (lambda () nil)))
          (mevedel-view-agent-status-activate-row))
        (should (plist-get (mevedel-view--agent-activity-state agent-id)
                           :expanded)))))

  :doc "terminal row reveal does not open transcript"
  (mevedel-view-test--with-buffers
    (let* ((agent-id "explorer--done123")
           (workspace (mevedel-workspace--create
                       :type 'project
                       :id "status-reveal-terminal"
                       :root temporary-file-directory
                       :name "status-reveal-terminal"))
           (session (mevedel-session-create "main" workspace))
           opened)
      (setf (mevedel-session-agent-transcripts session)
            (list (cons agent-id '(:status completed))))
      (with-current-buffer data-buf
        (setq-local mevedel--session session))
      (with-current-buffer view-buf
        (let ((inhibit-read-only t))
          (goto-char mevedel-view--input-marker)
          (insert (propertize "handle\n"
                              'mevedel-view-agent-id agent-id
                              'mevedel-view-agent-handle-p t))
          (insert (mevedel-view--agent-status-row-string
                   (list :agent-id agent-id :status 'completed))))
        (goto-char (point-min))
        (search-forward "explorer--done123")
        (cl-letf (((symbol-function 'mevedel-view-open-agent-transcript)
                   (lambda (&rest _) (setq opened t))))
          (mevedel-view-agent-status-activate-row))
        (should-not opened)))))

(mevedel-deftest mevedel-view--insert-attribution
  (:doc "builds transcript attribution fragments")
  ,test
  (test)

  :doc "uses short display label and no keymap when no sidecar entry exists"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((s (mevedel-view--insert-attribution
                 "explorer--abcdef1234567890"))
             (pos (string-match-p "explorer--abcdef12" s)))
        (should (string-match-p "from explorer--abcdef12" s))
        (should pos)
        (should-not (get-text-property pos 'keymap s))
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
