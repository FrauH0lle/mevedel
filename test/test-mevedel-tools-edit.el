;;; test-mevedel-tools-edit.el --- Tests for mevedel Edit tool -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the mevedel Edit tool, including:
;; - String replacement mode
;; - Inline preview creation and content verification
;; - Change application after approval
;; - Markdown code block replacement (regression test)

;;; Code:

(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;;; Test Configuration

;; Force inline preview mode for all tests by setting a very high threshold
(defvar mevedel-test-edit--original-threshold nil
  "Original inline preview threshold value.")

;; Mock interactive approval to auto-approve in non-interactive tests
(defvar mevedel-test-edit--auto-approve nil
  "When non-nil, automatically approve changes without prompting.")

;; Silence flycheck warnings for buffer-local variables from mevedel-tools.el
(defvar mevedel--root)
(defvar mevedel--real-path)
(defvar mevedel--final-callback)

(defun mevedel-test-edit--auto-approve-diff-buffer ()
  "Auto-approve diff in separate buffer mode without prompting.
This is used as advice around `read-char-choice' during tests."
  (let ((diff-buffer (get-buffer "*mevedel-diff-preview*")))
    (when diff-buffer
      (with-current-buffer diff-buffer
        ;; Get required buffer-local variables
        (let ((default-directory (or mevedel--root default-directory))
              (real-path mevedel--real-path)
              (final-callback mevedel--final-callback))
          ;; Apply the diff
          (mevedel-diff-apply-buffer)
          ;; Call the callback
          (when final-callback
            (funcall final-callback
                     (format "Changes approved and applied to %s" real-path)))))))
  ;; Return 'a' to simulate approval
  ?a)

(defun mevedel-test-edit--read-char-choice-advice (orig-fun prompt choices &rest args)
  "Advice for `read-char-choice' to auto-approve during testing.
ORIG-FUN is the original function.
PROMPT is the prompt string.
CHOICES are the valid choices.
ARGS are additional arguments."
  (if (and noninteractive
           (string-match-p "Apply changes" prompt))
      ;; In non-interactive mode with diff approval prompt, auto-approve
      (progn
        (message "Auto-approving diff changes (test mode)")
        (mevedel-test-edit--auto-approve-diff-buffer))
    ;; Otherwise use original function
    (apply orig-fun prompt choices args)))

;;; Test Helper Functions

(defun mevedel-test--find-inline-preview-overlay (buffer)
  "Find and return the inline preview overlay in BUFFER, or nil if not found."
  (with-current-buffer buffer
    (let ((found-overlay nil))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'mevedel-inline-preview)
          (setq found-overlay ov)))
      found-overlay)))

(defun mevedel-test--get-overlay-text (overlay)
  "Get the text content of OVERLAY."
  (when overlay
    (when-let ((buf (overlay-buffer overlay)))
      (with-current-buffer buf
        (buffer-substring-no-properties (overlay-start overlay)
                                        (overlay-end overlay))))))

(defun mevedel-test--count-substring (string substring)
  "Count occurrences of SUBSTRING in STRING."
  (let ((count 0)
        (pos 0))
    (while (string-match (regexp-quote substring) string pos)
      (setq count (1+ count)
            pos (match-end 0)))
    count))

;;; Tests

(mevedel-deftest mevedel-tools-edit-simple-replacement
  (:vars* ((test-dir (make-temp-file "mevedel-test-" t))
           (test-file (expand-file-name "simple.txt" test-dir))
           (chat-buffer (generate-new-buffer " *test-edit-simple*"))
           (workspace (cons 'file test-dir))
           (original-content "Line 1\nLine 2\nLine 3\n")
           (old-str "Line 2")
           (new-str "Line 2 Modified")
           (diff-buffer-name "*mevedel-diff-preview*")
           (original-threshold mevedel-inline-preview-threshold))
   :vars ((callback-invoked nil)
          (callback-result nil))
   :before-each
   (progn
     ;; Setup - force inline preview mode and auto-approve
     (setq mevedel-inline-preview-threshold 2.0  ; Very high to always use inline
           callback-invoked nil
           callback-result nil)
     ;; Add advice to auto-approve if separate buffer mode is used
     (advice-add 'read-char-choice :around #'mevedel-test-edit--read-char-choice-advice)
     (with-temp-file test-file
       (insert original-content))
     (with-current-buffer chat-buffer
       (setq-local mevedel--workspace workspace)
       (fundamental-mode))
     ;; Display buffer and ensure it has a reasonable window size
     ;; In batch mode, window resizing doesn't work, so ignore errors
     (let ((window (display-buffer chat-buffer)))
       (when window
         (ignore-errors (set-window-text-height window 50))))
     (sit-for 0.1)

     ;; Define callback
     (defun test-edit-simple-callback (result)
       (setq callback-invoked t
             callback-result result))

     ;; Execute Edit tool
     (with-current-buffer chat-buffer
       (mevedel-tools--edit-files-1
        #'test-edit-simple-callback
        test-file
        old-str
        new-str
        :json-false))

     (sit-for 0.5))

   :after-each
   (progn
     ;; Cleanup
     (advice-remove 'read-char-choice #'mevedel-test-edit--read-char-choice-advice)
     (setq mevedel-inline-preview-threshold original-threshold)
     (when (buffer-live-p chat-buffer)
       (kill-buffer chat-buffer))
     (when (get-buffer diff-buffer-name)
       (kill-buffer diff-buffer-name))
     (when (file-exists-p test-dir)
       (delete-directory test-dir t)))

   :doc "Test Edit tool with simple string replacement.

Verifies that:
1. Inline preview overlay is created
2. Overlay contains expected diff
3. File is modified correctly after approval")

  ,test
  (test)
  :doc "Inline preview overlay is created"
  (should (mevedel-test--find-inline-preview-overlay chat-buffer))

  :doc "Overlay contains expected diff content"
  (let* ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer))
         (overlay-text (mevedel-test--get-overlay-text overlay)))
    (should (string-match-p "Proposed changes" overlay-text))
    (should (string-match-p "simple.txt" overlay-text))
    (should (string-match-p "-Line 2" overlay-text))
    (should (string-match-p "\\+Line 2 Modified" overlay-text))
    (should (string-match-p "Keys:" overlay-text)))

  :doc "Diff buffer exists"
  (should (get-buffer diff-buffer-name))

  :doc "File is modified correctly after approval and callback is invoked"
  (let ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer)))
    (with-current-buffer chat-buffer
      (goto-char (overlay-start overlay))
      (mevedel-tools--approve-inline-preview))
    (sit-for 0.5)
    ;; Check file was modified correctly
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((final-content (buffer-string)))
        (should (string-match-p "Line 2 Modified" final-content))
        (should-not (string-match-p "^Line 2$" final-content))
        (should (string-match-p "Line 1" final-content))
        (should (string-match-p "Line 3" final-content))
        (should (= 3 (mevedel-test--count-substring final-content "\n")))))
    ;; Check callback was invoked with success message
    (should callback-invoked)
    (should (string-match-p "approved and applied" callback-result))))

(mevedel-deftest mevedel-tools-edit-markdown-code-block
  (:vars* ((test-dir (make-temp-file "mevedel-test-" t))
           (test-file (expand-file-name "test.md" test-dir))
           (chat-buffer (generate-new-buffer " *test-edit-markdown*"))
           (workspace (cons 'file test-dir))
           (original-content "# Document Title

### Testing
```bash
# Run unit tests with buttercup
emacs --script run-tests.el

# Or run tests in batch mode
emacs --batch -l run-tests.el
```
")
           (old-str "```bash
# Run unit tests with buttercup
emacs --script run-tests.el

# Or run tests in batch mode
emacs --batch -l run-tests.el
```")
           (new-str "```bash
# Run unit tests with ERT using Eask
eask test ert test/test-*

# Run tests with Eask via npx
npx @emacs-eask/cli test ert test/test-*
```")
           (diff-buffer-name "*mevedel-diff-preview*")
           (original-threshold mevedel-inline-preview-threshold))
   :vars ((callback-invoked nil)
          (callback-result nil))
   :before-each
   (progn
     ;; Setup - force inline preview mode and auto-approve
     (setq mevedel-inline-preview-threshold 2.0  ; Very high to always use inline
           callback-invoked nil
           callback-result nil)
     ;; Add advice to auto-approve if separate buffer mode is used
     (advice-add 'read-char-choice :around #'mevedel-test-edit--read-char-choice-advice)
     (with-temp-file test-file
       (insert original-content))
     (with-current-buffer chat-buffer
       (setq-local mevedel--workspace workspace)
       (org-mode))
     ;; Display buffer and ensure it has a reasonable window size
     ;; In batch mode, window resizing doesn't work, so ignore errors
     (let ((window (display-buffer chat-buffer)))
       (when window
         (ignore-errors (set-window-text-height window 50))))
     (sit-for 0.1)

     ;; Define callback
     (defun test-edit-markdown-callback (result)
       (setq callback-invoked t
             callback-result result))

     ;; Execute Edit tool
     (with-current-buffer chat-buffer
       (mevedel-tools--edit-files-1
        #'test-edit-markdown-callback
        test-file
        old-str
        new-str
        :json-false))

     (sit-for 0.5))

   :after-each
   (progn
     ;; Cleanup
     (advice-remove 'read-char-choice #'mevedel-test-edit--read-char-choice-advice)
     (setq mevedel-inline-preview-threshold original-threshold)
     (when (buffer-live-p chat-buffer)
       (kill-buffer chat-buffer))
     (when (get-buffer diff-buffer-name)
       (kill-buffer diff-buffer-name))
     (when (file-exists-p test-dir)
       (delete-directory test-dir t)))

   :doc "Test Edit tool with markdown code block replacement.

This is a regression test for a bug where replacing markdown code blocks
resulted in content being appended instead of replaced.")

  ,test
  (test)
  :doc "Inline preview overlay is created"
  (should (mevedel-test--find-inline-preview-overlay chat-buffer))

  :doc "Overlay contains expected diff markers"
  (let* ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer))
         (overlay-text (mevedel-test--get-overlay-text overlay)))
    (should (string-match-p "Proposed changes" overlay-text))
    (should (string-match-p "test.md" overlay-text))
    (should (string-match-p "-.*buttercup" overlay-text))
    (should (string-match-p "\\+.*ERT using Eask" overlay-text)))

  :doc "File is modified correctly - content is REPLACED, not appended, and callback is invoked"
  (let ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer)))
    (with-current-buffer chat-buffer
      (goto-char (overlay-start overlay))
      (mevedel-tools--approve-inline-preview))
    (sit-for 0.5)
    ;; Check file was modified correctly
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((final-content (buffer-string)))
        ;; Should contain new content
        (should (string-match-p "ERT using Eask" final-content))
        (should (string-match-p "eask test ert" final-content))
        ;; Should NOT contain old content
        (should-not (string-match-p "buttercup" final-content))
        (should-not (string-match-p "run-tests.el" final-content))
        ;; Should NOT be duplicated
        (should (= 1 (mevedel-test--count-substring final-content "### Testing")))
        (should (= 1 (mevedel-test--count-substring final-content "```bash")))
        (should (= 1 (mevedel-test--count-substring final-content "```\n")))
        ;; Should still have original structure
        (should (string-match-p "# Document Title" final-content))))
    ;; Check callback was invoked with success message
    (should callback-invoked)
    (should (string-match-p "approved and applied" callback-result))))

(mevedel-deftest mevedel-tools-edit-inline-preview-content
  (:vars* ((test-dir (make-temp-file "mevedel-test-" t))
           (test-file (expand-file-name "content.txt" test-dir))
           (chat-buffer (generate-new-buffer " *test-edit-content*"))
           (workspace (cons 'file test-dir))
           (original-content "Hello\nWorld\n")
           (old-str "World")
           (new-str "Emacs")
           (original-threshold mevedel-inline-preview-threshold))
   :before-each
   (progn
     ;; Setup - force inline preview mode and auto-approve
     (setq mevedel-inline-preview-threshold 2.0)  ; Very high to always use inline
     ;; Add advice to auto-approve if separate buffer mode is used
     (advice-add 'read-char-choice :around #'mevedel-test-edit--read-char-choice-advice)
     (with-temp-file test-file
       (insert original-content))
     (with-current-buffer chat-buffer
       (setq-local mevedel--workspace workspace))
     ;; Display buffer and ensure it has a reasonable window size
     ;; In batch mode, window resizing doesn't work, so ignore errors
     (let ((window (display-buffer chat-buffer)))
       (when window
         (ignore-errors (set-window-text-height window 50))))
     (sit-for 0.1)

     ;; Define callback
     (defun test-edit-content-callback (result)
       (ignore result))

     ;; Execute Edit tool
     (with-current-buffer chat-buffer
       (mevedel-tools--edit-files-1
        #'test-edit-content-callback
        test-file
        old-str
        new-str
        :json-false))

     (sit-for 0.5))

   :after-each
   (progn
     ;; Cleanup
     (advice-remove 'read-char-choice #'mevedel-test-edit--read-char-choice-advice)
     (setq mevedel-inline-preview-threshold original-threshold)
     (when (buffer-live-p chat-buffer)
       (kill-buffer chat-buffer))
     (when (get-buffer "*mevedel-diff-preview*")
       (kill-buffer "*mevedel-diff-preview*"))
     (when (file-exists-p test-dir)
       (delete-directory test-dir t)))

   :doc "Test that inline preview overlay contains all expected elements.")

  ,test
  (test)
  :doc "Overlay has required properties"
  (let ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer)))
    (should overlay)
    (should (overlay-get overlay 'mevedel--temp-file))
    (should (overlay-get overlay 'mevedel--real-path))
    (should (overlay-get overlay 'mevedel--final-callback))
    (should (overlay-get overlay 'mevedel--chat-buffer))
    (should (overlay-get overlay 'mevedel--workspace))
    (should (overlay-get overlay 'mevedel--root))
    (should (overlay-get overlay 'keymap)))

  :doc "Overlay text contains expected elements"
  (let* ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer))
         (overlay-text (when overlay
                         (with-current-buffer chat-buffer
                           (mevedel-test--get-overlay-text overlay)))))
    ;; Should have header
    (should (string-match-p "Edit:.*Proposed changes" overlay-text))
    (should (string-match-p "content.txt" overlay-text))
    ;; Should have diff content
    (should (string-match-p "@@.*@@" overlay-text)) ; Diff hunk header
    (should (string-match-p "-World" overlay-text))
    (should (string-match-p "\\+Emacs" overlay-text))
    ;; Should have help text
    (should (string-match-p "Keys:" overlay-text))
    (should (string-match-p "RET.*approve" overlay-text))
    (should (string-match-p "q.*reject" overlay-text))
    (should (string-match-p "e.*edit" overlay-text))
    (should (string-match-p "f.*feedback" overlay-text))
    (should (string-match-p "TAB.*toggle" overlay-text))))

(mevedel-deftest mevedel-tools-edit-rejection
  (:vars* ((test-dir (make-temp-file "mevedel-test-" t))
           (test-file (expand-file-name "reject.txt" test-dir))
           (chat-buffer (generate-new-buffer " *test-edit-reject*"))
           (workspace (cons 'file test-dir))
           (original-content "Original content\n")
           (old-str "Original")
           (new-str "Modified")
           (original-threshold mevedel-inline-preview-threshold))
   :vars ((callback-invoked nil)
          (callback-result nil))
   :before-each
   (progn
     ;; Setup - force inline preview mode and auto-approve
     (setq mevedel-inline-preview-threshold 2.0  ; Very high to always use inline
           callback-invoked nil
           callback-result nil)
     ;; Add advice to auto-approve if separate buffer mode is used
     (advice-add 'read-char-choice :around #'mevedel-test-edit--read-char-choice-advice)
     (with-temp-file test-file
       (insert original-content))
     (with-current-buffer chat-buffer
       (setq-local mevedel--workspace workspace))
     ;; Display buffer and ensure it has a reasonable window size
     ;; In batch mode, window resizing doesn't work, so ignore errors
     (let ((window (display-buffer chat-buffer)))
       (when window
         (ignore-errors (set-window-text-height window 50))))
     (sit-for 0.1)

     ;; Define callback
     (defun test-edit-reject-callback (result)
       (setq callback-invoked t
             callback-result result))

     ;; Execute Edit tool
     (with-current-buffer chat-buffer
       (mevedel-tools--edit-files-1
        #'test-edit-reject-callback
        test-file
        old-str
        new-str
        :json-false))

     (sit-for 0.5))

   :after-each
   (progn
     ;; Cleanup
     (advice-remove 'read-char-choice #'mevedel-test-edit--read-char-choice-advice)
     (setq mevedel-inline-preview-threshold original-threshold)
     (when (buffer-live-p chat-buffer)
       (kill-buffer chat-buffer))
     (when (get-buffer "*mevedel-diff-preview*")
       (kill-buffer "*mevedel-diff-preview*"))
     (when (file-exists-p test-dir)
       (delete-directory test-dir t)))

   :doc "Test that rejecting inline preview works correctly.")

  ,test
  (test)
  :doc "File is NOT modified after rejection and callback is invoked"
  (let ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer)))
    (should overlay)
    ;; Reject changes
    (with-current-buffer chat-buffer
      (goto-char (overlay-start overlay))
      (mevedel-tools--reject-inline-preview))
    (sit-for 0.2)
    ;; Verify file was NOT modified
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((final-content (buffer-string)))
        (should (string= final-content original-content))
        (should (string-match-p "Original content" final-content))
        (should-not (string-match-p "Modified" final-content))))
    ;; Check callback was invoked with rejection message
    (should callback-invoked)
    (should (string-match-p "rejected" callback-result))))

(mevedel-deftest mevedel-tools-edit-multiline-replacement
  (:vars* ((test-dir (make-temp-file "mevedel-test-" t))
           (test-file (expand-file-name "multiline.txt" test-dir))
           (chat-buffer (generate-new-buffer " *test-edit-multiline*"))
           (workspace (cons 'file test-dir))
           (original-content "Start\nLine A\nLine B\nLine C\nEnd\n")
           (old-str "Line A\nLine B\nLine C")
           (new-str "Replaced\nContent")
           (original-threshold mevedel-inline-preview-threshold))
   :vars ((callback-invoked nil))
   :before-each
   (progn
     ;; Setup - force inline preview mode and auto-approve
     (setq mevedel-inline-preview-threshold 2.0  ; Very high to always use inline
           callback-invoked nil)
     ;; Add advice to auto-approve if separate buffer mode is used
     (advice-add 'read-char-choice :around #'mevedel-test-edit--read-char-choice-advice)
     (with-temp-file test-file
       (insert original-content))
     (with-current-buffer chat-buffer
       (setq-local mevedel--workspace workspace))
     ;; Display buffer and ensure it has a reasonable window size
     ;; In batch mode, window resizing doesn't work, so ignore errors
     (let ((window (display-buffer chat-buffer)))
       (when window
         (ignore-errors (set-window-text-height window 50))))
     (sit-for 0.1)

     ;; Define callback
     (defun test-edit-multiline-callback (result)
       (ignore result)
       (setq callback-invoked t))

     ;; Execute Edit tool
     (with-current-buffer chat-buffer
       (mevedel-tools--edit-files-1
        #'test-edit-multiline-callback
        test-file
        old-str
        new-str
        :json-false))

     (sit-for 0.5))

   :after-each
   (progn
     ;; Cleanup
     (advice-remove 'read-char-choice #'mevedel-test-edit--read-char-choice-advice)
     (setq mevedel-inline-preview-threshold original-threshold)
     (when (buffer-live-p chat-buffer)
       (kill-buffer chat-buffer))
     (when (get-buffer "*mevedel-diff-preview*")
       (kill-buffer "*mevedel-diff-preview*"))
     (when (file-exists-p test-dir)
       (delete-directory test-dir t)))

   :doc "Test Edit tool with multi-line string replacement.")

  ,test
  (test)
  :doc "Multi-line content is replaced correctly and callback is invoked"
  (let ((overlay (mevedel-test--find-inline-preview-overlay chat-buffer)))
    (should overlay)
    ;; Approve changes
    (with-current-buffer chat-buffer
      (goto-char (overlay-start overlay))
      (mevedel-tools--approve-inline-preview))
    (sit-for 0.5)
    ;; Verify multi-line replacement worked correctly
    (with-temp-buffer
      (insert-file-contents test-file)
      (let ((final-content (buffer-string)))
        (should (string-match-p "Start" final-content))
        (should (string-match-p "Replaced" final-content))
        (should (string-match-p "Content" final-content))
        (should (string-match-p "End" final-content))
        ;; Old content should be gone
        (should-not (string-match-p "Line A" final-content))
        (should-not (string-match-p "Line B" final-content))
        (should-not (string-match-p "Line C" final-content))))
    ;; Check callback was invoked
    (should callback-invoked)))

(provide 'test-mevedel-tools-edit)
;;; test-mevedel-tools-edit.el ends here
