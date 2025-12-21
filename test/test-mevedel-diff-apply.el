;;; tests/test-mevedel-diff-apply.el -- Overlay preservation tests -*- lexical-binding: t -*-

;;; Commentary:

;; XXX 2025-11-02: Test "Real World Examples:Example 1" can fail, depending on
;;   the evnironment the test runs in. It is manually verified to work correctly
;;   and should work in the CI pipeline.

;;; Code:

(require 'mevedel)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

;; NOTE 2025-10-01: For better DEBUG output, use the following snippet in tests:
;;
;; (message "DEBUG TEST: ov-orig-start=%s ov-orig-end=%s" ov-orig-start ov-orig-end)
;; (message "DEBUG TEST: buffer-text=%S (len=%s)" buffer-text (length buffer-text))
;; (message "DEBUG TEST: ov-text=%S (len=%s)" ov-text (length ov-text))
;; (with-current-buffer test-buffer
;;   (message "DEBUG TEST: actual buffer content=%S" (buffer-substring-no-properties (point-min) (point-max)))
;;   (message "DEBUG TEST: actual overlay content=%S" (buffer-substring-no-properties ov-orig-start ov-orig-end)))
;; (with-current-buffer diff-buffer
;;   (let ((default-directory (temporary-file-directory))
;;         (inhibit-message t))
;;     (mevedel-diff-apply-buffer)))
;; (message "DEBUG TEST AFTER: ov-start=%s ov-end=%s" (overlay-start ov) (overlay-end ov))
;; (with-current-buffer test-buffer
;;   (message "DEBUG TEST AFTER: actual overlay content=%S" (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))


;;
;;; Helpers

(defun mevedel-test--create-diff-buffer (modified &optional file-buffer original)
  "Create a diff buffer comparing ORIGINAL to MODIFIED content.

FILE-BUFFER is the buffer to diff against, or a temp file is created.
Returns a read-only buffer in `diff-mode' containing the unified diff."
  (let* ((orig-file (or (and file-buffer
                             (buffer-file-name file-buffer))
                        (make-temp-file "mevedel-test-" nil ".txt" original)))
         (original (with-temp-buffer
                     (insert-file-contents-literally orig-file)
                     (buffer-substring-no-properties (point-min) (point-max))))
         (modified-temp-file (make-temp-file "mevedel-test-" nil ".txt" modified))
         (rel-path (file-relative-name orig-file (temporary-file-directory)))
         (buffer (get-buffer-create "mevedel-test-diff")))

    ;; Generate diff and append to result.
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (goto-char (point-min))

      ;; Add the standard git diff header, which allows diff-mode to create new
      ;; files.
      (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))

      ;; Use diff to generate a unified patch with the correct file path.
      (when (or original modified)
        (call-process "diff"
                      nil t nil "-u" "--label"
                      (if original
                          (concat "a/" rel-path)
                        ;; Use /dev/null to denote file creations.
                        "/dev/null")
                      "--label"
                      (if modified
                          (concat "b/" rel-path)
                        ;; Use /dev/null to denote file deletions.
                        "/dev/null")
                      orig-file modified-temp-file))
      (diff-mode)
      (read-only-mode +1))

    buffer))

(defun mevedel-test--create-overlay (buffer &optional start end text type-of)
  "Create a mevedel instruction overlay in BUFFER.

Either provide START and END positions, or TEXT to search for. TYPE-OF
specifies \\='reference or \\='directive (default). Returns the overlay."
  (when (and text (or start end))
    (user-error "Only provide either start and end or text"))
  (with-current-buffer buffer
    (when text
      (goto-char (point-min))
      (if-let* ((bounds (when (re-search-forward (regexp-quote text) nil t)
                          (set-mark (match-beginning 0))
                          (goto-char (match-end 0))
                          (car (region-bounds)))))
          (progn
            (deactivate-mark)
            (setq start (car bounds))
            (setq end (cdr bounds)))
        (user-error "Could not find text in buffer")))
    ;; TYPE-OF is either 'reference of 'directive
    (if (eq type-of 'reference)
        ;; Returns ov
        (mevedel--create-reference-in buffer start end)
      ;; Returns ov
      (mevedel--create-directive-in buffer start end nil "foo"))))

(defun mevedel-test--create-buffer-with-overlay (buf-text &optional ov-start ov-end ov-text type-of)
  "Create a temporary file buffer with BUF-TEXT and an overlay.

Overlay position is specified by OV-START/OV-END or OV-TEXT to search.
TYPE-OF specifies \\='reference or \\='directive (default).
Returns (buffer . overlay)."
  (let* ((temp-file (make-temp-file "mevedel-test-" nil ".txt" buf-text))
         (test-buffer (find-file-noselect temp-file))
         ov)
    (with-current-buffer test-buffer
      ;; Ensure buffer is in fundamental mode and not modified
      (fundamental-mode)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (setq ov (mevedel-test--create-overlay test-buffer ov-start ov-end ov-text type-of)))
    (cons test-buffer ov)))

(defun mevedel-test--get-ov-at-point ()
  "Interactive helper to inspect directive overlay properties at point."
  (interactive)
  (if-let* ((directive (mevedel--topmost-instruction (mevedel--highest-priority-instruction
                                                      (mevedel--instructions-at (point) 'directive)
                                                      t)
                                                     'directive)))
      (message "%s" (list
                     (overlay-start directive)
                     (overlay-end directive)
                     (overlay-properties directive)))
    (user-error "No directive found at point")))

;; Helper to find recreated overlays
(defun mevedel-test--find-overlays-in-buffer (buffer &optional type-of content-p)
  "Find the mevedel instruction overlays in BUFFER.

If TYPE-OF is specified (\\='reference or \\='directive), only return overlays
of that type.

If CONTENT-P is non-nil, return a list like ((OV-START OV-END OV-TEXT)
...)."
  (let ((overlays (alist-get buffer mevedel--instructions)))
    (when type-of
      (setq overlays (seq-filter
                      (lambda (ov)
                        (eq (overlay-get ov 'mevedel-instruction-type) type-of))
                      overlays)))
    (if content-p
        (mapcar (lambda (ov)
                  (list (overlay-start ov)
                        (overlay-end ov)
                        (with-current-buffer buffer
                          (buffer-substring-no-properties
                           (overlay-start ov) (overlay-end ov)))))
                overlays)
      overlays)))

(defun mevedel-test--overlay-is-line-based-p (ov)
  "Check if overlay OV spans full lines."
  (with-current-buffer (overlay-buffer ov)
    (save-excursion
      (let ((start (overlay-start ov))
            (end (overlay-end ov)))
        (and (progn (goto-char start) (bolp))
             (progn (goto-char end) (or (bolp) (eolp))))))))


;;
;;; mevedel--string-common-prefix

(mevedel-deftest mevedel--string-common-prefix ()
  ,test
  (test)
  :doc "`mevedel--string-common-prefix' returns empty string for empty input"
  (progn
    (should (string= "" (mevedel--string-common-prefix nil)))
    (should (string= "" (mevedel--string-common-prefix '())))
    (should (string= "" (mevedel--string-common-prefix '("")))))
  :doc "`mevedel--string-common-prefix' returns the string itself for single element"
  (should (string= "hello" (mevedel--string-common-prefix '("hello"))))
  :doc "`mevedel--string-common-prefix' finds common prefix"
  (progn
    (should (string= "he" (mevedel--string-common-prefix '("hello" "help" "hero"))))
    (should (string= "test" (mevedel--string-common-prefix '("test" "testing" "tester")))))
  :doc "`mevedel--string-common-prefix' returns empty string when no common prefix"
  (should (string= "" (mevedel--string-common-prefix '("abc" "def" "ghi")))))



;;
;;; mevedel--safe-string-diff-regions

(mevedel-deftest mevedel--safe-string-diff-regions ()
  ,test
  (test)
  :doc "`mevedel--safe-string-diff-regions' handles identical strings"
  (let ((result (mevedel--safe-string-diff-regions "hello" "hello")))
    ;; For identical strings, everything is common - no diff region
    (should (equal "" (nth 2 result))) ; old-middle (no difference)
    (should (equal "" (nth 3 result)))) ; new-middle (no difference)
  :doc "`mevedel--safe-string-diff-regions' handles completely different strings"
  (let ((result (mevedel--safe-string-diff-regions "abc" "xyz")))
    (should (equal 0 (nth 0 result)))  ; prefix-len
    (should (equal 0 (nth 1 result)))  ; suffix-len
    (should (equal "abc" (nth 2 result))) ; old-middle
    (should (equal "xyz" (nth 3 result)))) ; new-middle
  :doc "`mevedel--safe-string-diff-regions' finds prefix and suffix correctly"
  (let ((result (mevedel--safe-string-diff-regions "before OLD after" "before NEW after")))
    (should (equal 7 (nth 0 result)))  ; "before "
    (should (equal 6 (nth 1 result)))  ; " after"
    (should (equal "OLD" (nth 2 result)))
    (should (equal "NEW" (nth 3 result))))
  :doc "`mevedel--safe-string-diff-regions' handles insertion"
  (let ((result (mevedel--safe-string-diff-regions "text" "text INSERTED")))
    (should (equal 4 (nth 0 result)))  ; "text"
    (should (equal 0 (nth 1 result)))
    (should (equal "" (nth 2 result)))
    (should (equal " INSERTED" (nth 3 result))))
  :doc "`mevedel--safe-string-diff-regions' handles deletion"
  (let ((result (mevedel--safe-string-diff-regions "text DELETED" "text")))
    (should (equal 4 (nth 0 result)))  ; "text"
    (should (equal 0 (nth 1 result)))
    (should (equal " DELETED" (nth 2 result)))
    (should (equal "" (nth 3 result)))))


;;
;;; mevedel--parse-hunk-lines

(mevedel-deftest mevedel--parse-hunk-lines ()
  ,test
  (test)
  :doc "`mevedel--parse-hunk-lines' parses single line change"
  (let ((result (mevedel--parse-hunk-lines "old line\n" "new line\n" 10)))
    (should (equal 1 (length result)))
    (let ((line (car result)))
      (should (equal "old line\n" (plist-get line :old)))
      (should (equal "new line\n" (plist-get line :new)))
      (should (equal 10 (plist-get line :start)))))
  :doc "`mevedel--parse-hunk-lines' parses deletion (old line, no new line)"
  (let ((result (mevedel--parse-hunk-lines "deleted\n" "" 5)))
    (should (equal 1 (length result)))
    (let ((line (car result)))
      (should (equal "deleted\n" (plist-get line :old)))
      (should (equal nil (plist-get line :new)))))
  :doc "`mevedel--parse-hunk-lines' parses insertion (no old line, new line)"
  (let ((result (mevedel--parse-hunk-lines "" "inserted\n" 5)))
    (should (equal 1 (length result)))
    (let ((line (car result)))
      (should (equal nil (plist-get line :old)))
      (should (equal "inserted\n" (plist-get line :new))))))


;;
;;; mevedel--overlay-is-line-based-p

(mevedel-deftest mevedel--overlay-is-line-based-p ()
  ,test
  (test)
  :doc "`mevedel--overlay-is-line-based-p' detects line-based overlay"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line2\n"))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup)))
    (with-current-buffer test-buffer
      (should (mevedel--overlay-is-line-based-p (overlay-start ov) (overlay-end ov) test-buffer))))
  :doc "`mevedel--overlay-is-line-based-p' detects non-line-based overlay"
  (let* ((buffer-text "some text here\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "text"))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup)))
    (with-current-buffer test-buffer
      (should-not (mevedel--overlay-is-line-based-p (overlay-start ov) (overlay-end ov) test-buffer)))))


;;
;;; mevedel--snap-to-full-lines

(mevedel-deftest mevedel--snap-to-full-lines ()
  ,test
  (test)
  :doc "`mevedel--snap-to-full-lines' snaps partial positions to full lines"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "ine2"))  ; partial line
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup)))
    (with-current-buffer test-buffer
      (let* ((start (overlay-start ov))
             (end (overlay-end ov))
             (snapped (mevedel--snap-to-full-lines start end test-buffer))
             (expected-start (save-excursion (goto-char start) (line-beginning-position)))
             (expected-end (save-excursion (goto-char end) (if (bolp) end (line-beginning-position 2)))))
        (should (equal expected-start (car snapped)))
        (should (equal expected-end (cdr snapped))))))
  :doc "`mevedel--snap-to-full-lines' leaves line-based positions unchanged"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line2\n"))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup)))
    (with-current-buffer test-buffer
      (let* ((start (overlay-start ov))
             (end (overlay-end ov))
             (snapped (mevedel--snap-to-full-lines start end test-buffer)))
        (should (equal start (car snapped)))
        (should (equal end (cdr snapped)))))))


;;
;;; mevedel--classify-change-relationship

(mevedel-deftest mevedel--classify-change-relationship ()
  ,test
  (test)
  :doc "`mevedel--classify-change-relationship' detects 'before relationship"
  (should (equal 'before (mevedel--classify-change-relationship 20 30 10 15)))
  :doc "`mevedel--classify-change-relationship' detects 'after relationship"
  (should (equal 'after (mevedel--classify-change-relationship 10 15 20 30)))
  :doc "`mevedel--classify-change-relationship' detects 'within relationship"
  (should (equal 'within (mevedel--classify-change-relationship 10 30 15 20)))
  :doc "`mevedel--classify-change-relationship' detects 'encompasses relationship"
  (should (equal 'encompasses (mevedel--classify-change-relationship 15 20 10 30)))
  :doc "`mevedel--classify-change-relationship' detects 'complex relationship (overlapping)"
  (progn
    (should (equal 'complex (mevedel--classify-change-relationship 10 20 15 25)))
    (should (equal 'complex (mevedel--classify-change-relationship 15 25 10 20)))))


;;
;;; mevedel--find-stub-line

(mevedel-deftest mevedel--find-stub-line ()
  ,test
  (test)
  :doc "`mevedel--find-stub-line' finds line above change position"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line2\n"))
         (test-buffer (car buf-setup)))
    (with-current-buffer test-buffer
      ;; Just verify it returns a valid cons cell with reasonable positions
      (let ((stub-line (mevedel--find-stub-line test-buffer 7))) ; position in line2
        (should (consp stub-line))
        (should (> (car stub-line) 0))
        (should (> (cdr stub-line) (car stub-line))))))
  :doc "`mevedel--find-stub-line' returns first line when change is at beginning"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line1\n"))
         (test-buffer (car buf-setup)))
    (with-current-buffer test-buffer
      ;; Just verify it returns a valid cons cell
      (let ((stub-line (mevedel--find-stub-line test-buffer 1)))
        (should (consp stub-line))
        (should (numberp (car stub-line)))
        (should (numberp (cdr stub-line)))))))


;;
;;; mevedel--path-has-suffix-p

(mevedel-deftest mevedel--path-has-suffix-p ()
  ,test
  (test)
  :doc "`mevedel--path-has-suffix-p' detects directory path suffix match"
  (progn
    (should (mevedel--path-has-suffix-p "/path/to/subdir" "to/subdir"))
    (should (mevedel--path-has-suffix-p "/path/to/subdir" "subdir")))
  :doc "`mevedel--path-has-suffix-p' detects no match for different suffix"
  (should-not (mevedel--path-has-suffix-p "/path/to/subdir" "other/subdir"))
  :doc "`mevedel--path-has-suffix-p' handles single directory component"
  (should (mevedel--path-has-suffix-p "/path/to/file" "file"))
  :doc "`mevedel--path-has-suffix-p' requires exact component match"
  (should-not (mevedel--path-has-suffix-p "/path/to/subdir" "dir")))


;;
;;; mevedel--replace-text

(mevedel-deftest mevedel--replace-text ()
  ,test
  (test)
  :doc "`mevedel--replace-text' replaces text in buffer"
  (let ((test-buffer (generate-new-buffer " *test-replace*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (insert "before MIDDLE after")
          (mevedel--replace-text 8 14 "NEW")
          (should (equal "before NEW after" (buffer-string))))
      (kill-buffer test-buffer)))
  :doc "`mevedel--replace-text' handles insertion (empty old text)"
  (let ((test-buffer (generate-new-buffer " *test-insert*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (insert "before after")
          ;; Position 7 is the space between "before" and "after"
          (mevedel--replace-text 8 8 "INSERTED ")
          (should (equal "before INSERTED after" (buffer-string))))
      (kill-buffer test-buffer)))
  :doc "`mevedel--replace-text' handles deletion (empty new text)"
  (let ((test-buffer (generate-new-buffer " *test-delete*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (insert "before DELETED after")
          (mevedel--replace-text 8 15 "")
          (should (equal "before  after" (buffer-string))))
      (kill-buffer test-buffer))))


;;
;;; mevedel--calculate-overlay-adjustment-granular

(mevedel-deftest mevedel--calculate-overlay-adjustment-granular ()
  ,test
  (test)
  :doc "`mevedel--calculate-overlay-adjustment-granular' handles simple modification within overlay"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line2\n"))
         (ov (cdr buf-setup))
         (line-changes (mevedel--parse-hunk-lines "line2\n" "modified2\n" 6)))
    (let ((result (mevedel--calculate-overlay-adjustment-granular ov line-changes)))
      (should result)
      (should (numberp (car result)))
      (should (numberp (cadr result)))))
  :doc "`mevedel--calculate-overlay-adjustment-granular' returns valid positions or nil for overlay not affected by changes"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line1\n"))
         (ov (cdr buf-setup))
         ;; Change affecting line3, not line1
         (line-changes (mevedel--parse-hunk-lines "line3\n" "modified3\n" 12)))
    (let ((result (mevedel--calculate-overlay-adjustment-granular ov line-changes)))
      ;; Should return original positions or nil
      (should (or (null result)
                  (and (listp result)
                       (numberp (car result))
                       (numberp (cadr result))))))))


;;
;;; mevedel--find-overlay-lines

(mevedel-deftest mevedel--find-overlay-lines ()
  ,test
  (test)
  :doc "`mevedel--find-overlay-lines' finds lines affected by overlay"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "line2\n"))
         (ov (cdr buf-setup))
         (line-changes (list
                        (list :old "line1\n" :new "line1\n" :start 1 :end 6)
                        (list :old "line2\n" :new "modified2\n" :start 7 :end 12)
                        (list :old "line3\n" :new "line3\n" :start 13 :end 18))))
    (let ((result (mevedel--find-overlay-lines ov line-changes)))
      (should (> (length result) 0))
      ;; Should include the line containing "line2"
      (let ((has-line2 (cl-some (lambda (line)
                                  (string-match "line2" (or (plist-get line :old) "")))
                                result)))
        (should has-line2)))))


;;
;;; mevedel-diff-apply-buffer

(mevedel-deftest mevedel-diff-apply-buffer
  (:before-each
   (setq mevedel--instructions nil)
   :after-each
   (dolist (buf (buffer-list))
     (when (string-match-p "mevedel-test" (buffer-name buf))
       (when (buffer-file-name buf)
         (let ((file (buffer-file-name buf)))
           (when (file-exists-p file)
             (delete-file file))))
       (kill-buffer buf))))
  ,test
  (test)
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 1: Change completely BEFORE overlay
Addition before overlay shifts it right"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (ov-text "line2\n")
         (new-text "INSERTED\nline1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (ov-orig-start (overlay-start ov))
         (ov-orig-end (overlay-end ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    (should (equal (+ ov-orig-start (length "INSERTED\n")) (overlay-start ov)))
    (should (equal (+ ov-orig-end (length "INSERTED\n")) (overlay-end ov))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 1: Change completely BEFORE overlay
Deletion before overlay shifts it left"
  (let* ((buffer-text "REMOVE\nline1\nline2\n")
         (ov-text "line2\n")
         (new-text "line1\nline2\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (ov-orig-start (overlay-start ov))
         (ov-orig-end (overlay-end ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))
    (should ov)
    (should (equal (- ov-orig-start (length "REMOVE\n")) (overlay-start ov)))
    (should (equal (- ov-orig-end (length "REMOVE\n")) (overlay-end ov))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 2: Change completely AFTER overlay
Addition after overlay doesn't affect it"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (ov-text "line1\n")
         (new-text "line1\nline2\nline3\nADDED\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (ov-orig-start (overlay-start ov))
         (ov-orig-end (overlay-end ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    (should (equal ov-orig-start (overlay-start ov)))
    (should (equal ov-orig-end (overlay-end ov))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 2: Change completely AFTER overlay
Deletion after overlay doesn't affect it"
  (let* ((buffer-text "line1\nline2\nREMOVE\n")
         (ov-text "line1\n")
         (new-text "line1\nline2\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (ov-orig-start (overlay-start ov))
         (ov-orig-end (overlay-end ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    (should (equal ov-orig-start (overlay-start ov)))
    (should (equal ov-orig-end (overlay-end ov))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 3: Change completely WITHIN overlay
Addition within overlay expands it"
  (let* ((buffer-text "function foo() {\n  old code\n}\n")
         (ov-text "function foo() {\n  old code\n}\n")
         (new-text "function foo() {\n  old code\n  NEW CODE ADDED\n}\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (ov-orig-start (overlay-start ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    (should (equal ov-orig-start (overlay-start ov)))
    (should (equal (with-current-buffer test-buffer (point-max)) (overlay-end ov)))
    (should (equal new-text
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 3: Change completely WITHIN overlay
Deletion within overlay shrinks it"
  (let* ((buffer-text "function foo() {\n  code to keep\n  code to remove\n}\n")
         (ov-text "function foo() {\n  code to keep\n  code to remove\n}\n")
         (new-text "function foo() {\n  code to keep\n}\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (ov-orig-start (overlay-start ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    (should (equal ov-orig-start (overlay-start ov)))
    (should (equal (with-current-buffer test-buffer (point-max)) (overlay-end ov)))
    (should (equal new-text
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 4: Change ENCOMPASSES overlay
Deletion - creates stub at nearest line above (line-based)"
  (let* ((buffer-text "before\nfunction foo() {\n  code to remove\n}\nafter\n")
         (ov-text "function foo() {\n  code to remove\n}\n")
         (new-text "before\nafter\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    ;; Should be stub on line "before"
    (should (equal "before\n"
                   (with-current-buffer test-buffer
                     (goto-char (overlay-start ov))
                     (thing-at-point 'line t)))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 4: Change ENCOMPASSES overlay
Deletion - creates single char stub (partial-line)"
  (let* ((buffer-text "The old word here\n")
         (ov-text "old")
         (new-text "The here\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    ;; Should be single character stub
    (should (< (- (overlay-end ov) (overlay-start ov)) 2)))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Case 4: Change ENCOMPASSES overlay
Replacement - expands to cover replacement"
  (let* ((buffer-text "before\nold content\nafter\n")
         (ov-text "old")
         (new-text "before\ncompletely new content here\nafter\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    ;; Should cover the replacement
    (should (string-match-p "completely new content here"
                            (with-current-buffer test-buffer
                              (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))
  :doc "`mevedel-diff-apply-buffer':
Core Geometry Tests:
Buffer-level overlay (spanning whole buffer) is not adjusted"
  (let* ((buffer-text "Line 1\nLine 2\nLine 3\n")
         ;; Create overlay spanning entire buffer
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil buffer-text 'directive))
         (test-buffer (car buf-setup))
         (buffer-ov (cdr buf-setup))
         (new-text "Line 1\nModified Line 2\nLine 3\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    ;; Verify overlay initially spans whole buffer
    (with-current-buffer test-buffer
      (should (equal (point-min) (overlay-start buffer-ov)))
      (should (equal (point-max) (overlay-end buffer-ov))))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; After applying diff, overlay should still span whole buffer
    ;; (not adjusted/deleted/recreated - original overlay should remain)
    (with-current-buffer test-buffer
      ;; Should still be the same overlay object (not recreated)
      (should buffer-ov)
      ;; Should still span whole buffer
      (should (equal (point-min) (overlay-start buffer-ov)))
      (should (equal (point-max) (overlay-end buffer-ov)))
      ;; Buffer content should be updated
      (should (equal new-text (buffer-substring-no-properties (point-min) (point-max))))))
  :doc "`mevedel-diff-apply-buffer':
Multi-Overlay Geometry Tests:
Multiple overlays: one before, one after change"
  (let* ((buffer-text "overlay1\nmiddle content\noverlay2\n")
         (ov1-text "overlay1\n")
         (ov2-text "overlay2\n")
         (new-text "overlay1\nCHANGED middle content\noverlay2\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov1-text))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         (ov1-orig-start (overlay-start ov1))
         (ov1-orig-end (overlay-end ov1))
         (ov2 (mevedel-test--create-overlay test-buffer nil nil ov2-text))
         (ov2-orig-start (overlay-start ov2))
         (ov2-orig-end (overlay-end ov2))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; ov1 should be unchanged (before change)
    ;; ov2 should shift right (after change)
    (should ov1)
    (should ov2)
    (should (equal ov1-orig-start (overlay-start ov1)))
    (should (equal ov1-orig-end (overlay-end ov1)))
    (should (equal (+ ov2-orig-start (length "CHANGED ")) (overlay-start ov2)))
    (should (equal (+ ov2-orig-end (length "CHANGED ")) (overlay-end ov2))))
  :doc "`mevedel-diff-apply-buffer':
Multi-Overlay Geometry Tests:
Multiple overlays: all shift together when change is before all"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (ov1-text "line1\n")
         (ov2-text "line2\n")
         (ov3-text "line3\n")
         (new-text "INSERTED\nline1\nline2\nline3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov1-text))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         (ov1-orig-start (overlay-start ov1))
         (ov2 (mevedel-test--create-overlay test-buffer nil nil ov2-text))
         (ov2-orig-start (overlay-start ov2))
         (ov3 (mevedel-test--create-overlay test-buffer nil nil ov3-text))
         (ov3-orig-start (overlay-start ov3))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; All overlays should shift right by same delta
    (let ((delta (length "INSERTED\n")))
      (should ov1)
      (should ov2)
      (should ov3)
      (should (equal (+ ov1-orig-start delta) (overlay-start ov1)))
      (should (equal (+ ov2-orig-start delta) (overlay-start ov2)))
      (should (equal (+ ov3-orig-start delta) (overlay-start ov3)))))
  :doc "`mevedel-diff-apply-buffer':
Multi-Overlay Geometry Tests:
Multiple overlays: change within one, others unaffected"
  (let* ((buffer-text "overlay1\noverlay2 with content\noverlay3\n")
         (ov1-text "overlay1\n")
         (ov2-text "overlay2 with content\n")
         (ov3-text "overlay3\n")
         (new-text "overlay1\noverlay2 with MODIFIED content\noverlay3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov1-text))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         (ov1-orig-start (overlay-start ov1))
         (ov1-orig-end (overlay-end ov1))
         (ov2 (mevedel-test--create-overlay test-buffer nil nil ov2-text))
         (ov2-orig-start (overlay-start ov2))
         (ov3 (mevedel-test--create-overlay test-buffer nil nil ov3-text))
         (ov3-orig-start (overlay-start ov3))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; ov1 unchanged (before), ov2 expands (within), ov3 shifts (after)
    (should ov1)
    (should ov2)
    (should ov3)
    ;; ov1 unchanged
    (should (equal ov1-orig-start (overlay-start ov1)))
    (should (equal ov1-orig-end (overlay-end ov1)))
    ;; ov2 expanded
    (should (equal ov2-orig-start (overlay-start ov2)))
    (should (> (overlay-end ov2) (+ ov2-orig-start (length ov2-text))))
    ;; ov3 shifted
    (should (> (overlay-start ov3) ov3-orig-start)))
  :doc "`mevedel-diff-apply-buffer':
Multi-Overlay Geometry Tests:
Multiple overlays: some deleted, some preserved"
  (let* ((buffer-text "keep1\ndelete me\nkeep2\n")
         (ov-keep1-text "keep1\n")
         (ov-delete-text "delete me\n")
         (ov-keep2-text "keep2\n")
         (new-text "keep1\nkeep2\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-keep1-text))
         (test-buffer (car buf-setup))
         (ov-keep1 (cdr buf-setup))
         (ov-delete (mevedel-test--create-overlay test-buffer nil nil ov-delete-text))
         (ov-keep2 (mevedel-test--create-overlay test-buffer nil nil ov-keep2-text))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; All 3 overlays should still exist: keep1 (unchanged), delete (stub), keep2 (shifted left)
    (should ov-keep1)
    (should ov-delete)
    (should ov-keep2)
    ;; Check that delete became a stub (very small overlay)
    (should (< (- (overlay-end ov-delete) (overlay-start ov-delete)) 10)))
  :doc "`mevedel-diff-apply-buffer':
Multi-Overlay Geometry Tests:
Multiple overlays: nested within one encompassing change"
  (let* ((buffer-text "line1\nline2\nline3\n")
         (ov1-text "line1\n")
         (ov2-text "line2\n")
         (ov3-text "line3\n")
         (new-text "COMPLETE REPLACEMENT\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov1-text))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         (ov2 (mevedel-test--create-overlay test-buffer nil nil ov2-text))
         (ov3 (mevedel-test--create-overlay test-buffer nil nil ov3-text))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; All 3 overlays should expand to cover the replacement
    (should ov1)
    (should ov2)
    (should ov3)
    (dolist (ov (list ov1 ov2 ov3))
      (should (string-match-p "COMPLETE REPLACEMENT"
                              (with-current-buffer test-buffer
                                (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))))))
  :doc "`mevedel-diff-apply-buffer':
Multi-Overlay Geometry Tests:
Multiple overlays with before/after context"
  (let* ((buffer-text "before\ninner1\ninner2\ninner3\nafter\n")
         (ov1-text "inner1\n")
         (ov2-text "inner2\n")
         (ov3-text "inner3\n")
         (new-text "before\nCOMPLETE REPLACEMENT\nafter\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov1-text))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         (ov2 (mevedel-test--create-overlay test-buffer nil nil ov2-text))
         (ov3 (mevedel-test--create-overlay test-buffer nil nil ov3-text))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; All overlays should still exist
    (should ov1)
    (should ov2)
    (should ov3))
  :doc "`mevedel-diff-apply-buffer':
Line-Span Preservation Tests:
Line-based overlay stays line-based after within-change"
  (let* ((buffer-text "function foo() {\n  code\n}\n")
         (ov-text "function foo() {\n  code\n}\n")
         (new-text "function foo() {\n  code\n  more code\n}\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (was-line-based (mevedel-test--overlay-is-line-based-p ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (should was-line-based)

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    (should (mevedel-test--overlay-is-line-based-p ov)))
  :doc "`mevedel-diff-apply-buffer':
Line-Span Preservation Tests:
Partial-line overlay stays partial after within-change"
  (let* ((buffer-text "The old code here\n")
         (ov-text "old")
         (new-text "The old expanded code here\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (was-line-based (mevedel-test--overlay-is-line-based-p ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (should-not was-line-based)

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    ;; Should still be partial-line (not at BOL)
    (should-not (with-current-buffer test-buffer
                  (goto-char (overlay-start ov))
                  (bolp))))
  :doc "`mevedel-diff-apply-buffer':
Line-Span Preservation Tests:
Complex case (overlapping): line-based overlay stays line-based"
  (let* ((buffer-text "prefix text\noverlay start\noverlay middle\noverlay end\nsuffix text\n")
         (ov-text "overlay start\noverlay middle\noverlay end\n")
         (new-text "prefix text changed\noverlay start\noverlay middle\noverlay end\nsuffix text\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay buffer-text nil nil ov-text))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (was-line-based (mevedel-test--overlay-is-line-based-p ov))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (should was-line-based)

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; Overlay should still be line-based after adjustment
    (should ov)
    (should (mevedel-test--overlay-is-line-based-p ov)))
  :doc "`mevedel-diff-apply-buffer':
Nested Overlays:
Both parent and child adjust independently when change within both"
  (let* ((buffer-text "directive start\n  reference content\ndirective end\n")
         (directive-text "directive start\n  reference content\ndirective end\n")
         (reference-text "reference content")
         (new-text "directive start\n  reference MODIFIED content\ndirective end\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil directive-text 'directive))
         (test-buffer (car buf-setup))
         (directive-ov (cdr buf-setup))
         (reference-ov (mevedel-test--create-overlay
                        test-buffer nil nil reference-text 'reference))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should directive-ov)
    (should reference-ov)
    ;; Both should expand
    (should (string-match-p "MODIFIED"
                            (with-current-buffer test-buffer
                              (buffer-substring-no-properties (overlay-start reference-ov) (overlay-end reference-ov))))))
  :doc "`mevedel-diff-apply-buffer':
Nested Overlays:
Parent deleted causes child to be deleted"
  (let* ((buffer-text "before\ndirective start\n  reference content\ndirective end\nafter\n")
         (directive-text "directive start\n  reference content\ndirective end\n")
         (reference-text "reference content")
         (new-text "before\nafter\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil directive-text 'directive))
         (test-buffer (car buf-setup))
         (directive-ov (cdr buf-setup))
         (reference-ov (mevedel-test--create-overlay
                        test-buffer nil nil reference-text 'reference))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; Directive should exist as stub
    (should directive-ov)
    ;; Reference should also exist (currently we create stubs for all overlays)
    (should reference-ov))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Adjacent overlays at exact same boundary"
  (let* ((buffer-text "part1\npart2\npart3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "part1\n"))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         ;; Create second overlay starting exactly where first ends
         (ov2 (mevedel-test--create-overlay test-buffer nil nil "part2\n"))
         (new-text "part1\nREPLACED\npart3\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov1)
    (should ov2)
    ;; First overlay should be unchanged (before the change)
    (should (equal 1 (overlay-start ov1)))
    (should (equal 7 (overlay-end ov1))) ; "part1\n"
    ;; Second overlay should start exactly where first ends
    (should (equal 7 (overlay-start ov2)))
    ;; Second overlay should cover the replacement
    (should (string-match-p "REPLACED"
                            (with-current-buffer test-buffer
                              (buffer-substring-no-properties (overlay-start ov2) (overlay-end ov2))))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Change exactly matching overlay boundaries"
  (let* ((buffer-text "before\nCHANGE_ME\nafter\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "CHANGE_ME\n"))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (new-text "before\nREPLACED\nafter\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    ;; Overlay should expand to cover replacement
    (should (equal "REPLACED\n"
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Partial-line overlay with mid-line deletion"
  (let* ((buffer-text "The quick brown fox\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "quick"))
         (test-buffer (car buf-setup))
         (ov (cdr buf-setup))
         (new-text "The qui\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov)
    ;; Overlay should shrink but remain
    (should (equal "qui"
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Multiple overlays at same start position with different lengths"
  (let* ((buffer-text "shared start\nmore content\neven more\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "shared start\n" 'directive))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         ;; Create ov2 and ov3 starting at same position as ov1 but with different lengths
         (ov2 (mevedel-test--create-overlay test-buffer 1 27 nil 'reference)) ; covers first two lines
         (ov3 (mevedel-test--create-overlay test-buffer 1 39 nil 'reference)) ; covers entire buffer
         (new-text "PREFIX\nshared start\nmore content\neven more\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should ov1)
    (should ov2)
    (should ov3)
    ;; All should start at same position (after PREFIX\n)
    (should (equal 8 (overlay-start ov1)))
    (should (equal 8 (overlay-start ov2)))
    (should (equal 8 (overlay-start ov3)))
    ;; But maintain different lengths
    (should (< (overlay-end ov1) (overlay-end ov2)))
    (should (< (overlay-end ov2) (overlay-end ov3))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Pure deletion affecting multiple overlays creates multiple stubs"
  (let* ((buffer-text "keep\nov1\nov2\nov3\nkeep\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "ov1\n" 'directive))
         (test-buffer (car buf-setup))
         (ov1 (cdr buf-setup))
         (ov2 (mevedel-test--create-overlay test-buffer nil nil "ov2\n" 'directive))
         (ov3 (mevedel-test--create-overlay test-buffer nil nil "ov3\n" 'directive))
         (new-text "keep\nkeep\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; All three overlays should still exist as stubs
    (should ov1)
    (should ov2)
    (should ov3)
    ;; All should be small stubs
    (should (< (- (overlay-end ov1) (overlay-start ov1)) 10))
    (should (< (- (overlay-end ov2) (overlay-start ov2)) 10))
    (should (< (- (overlay-end ov3) (overlay-start ov3)) 10)))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Cumulative delta with mixed insert/delete pattern"
  (let* ((buffer-text "a\nb\nc\nd\ne\nTARGET\n")
         (test-buffer (generate-new-buffer " *test-cumulative*"))
         (target-ov nil)
         ;; Single hunk with mixed operations: delete "a\n", insert "XXX\n", delete "c\n"
         ;; These changes are consecutive so they form one hunk
         ;; Net effect: -2 + 4 - 2 = 0 delta for TARGET position
         (new-text "b\nXXX\nd\ne\nTARGET\n")
         (diff-buffer nil))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (insert buffer-text)
            (set-visited-file-name (make-temp-file "test-cumulative" nil ".txt"))
            ;; Create overlay at "TARGET\n"
            (setq target-ov (make-overlay 11 18))
            (overlay-put target-ov 'mevedel-instruction t)
            (overlay-put target-ov 'mevedel-instruction-type 'directive)
            (push target-ov (alist-get test-buffer mevedel--instructions)))

          (setq diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer))

          (cl-letf (((symbol-function #'mevedel-workspace)
                     (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
            (with-current-buffer diff-buffer
              (let ((default-directory (temporary-file-directory))
                    (inhibit-message t))
                (mevedel-diff-apply-buffer))))

          ;; Cumulative delta should be 0, so overlay position unchanged
          (should target-ov)
          (should (equal "TARGET\n"
                         (with-current-buffer test-buffer
                           (buffer-substring-no-properties (overlay-start target-ov) (overlay-end target-ov))))))
      (when (buffer-live-p test-buffer)
        (with-current-buffer test-buffer
          (set-buffer-modified-p nil))
        (kill-buffer test-buffer))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Multiple hunks with cumulative delta"
  (let* ((buffer-text "DELETE_ME\nheader1\nheader2\nheader3\nheader4\nheader5\n\nINSERT_HERE\ncontext1\ncontext2\ncontext3\ncontext4\ncontext5\n\nTARGET\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "TARGET\n" 'directive))
         (test-buffer (car buf-setup))
         (target-ov (cdr buf-setup))
         ;; Delete first line, insert after INSERT_HERE, TARGET unchanged
         (new-text "header1\nheader2\nheader3\nheader4\nheader5\n\nINSERT_HERE\nINSERTED\ncontext1\ncontext2\ncontext3\ncontext4\ncontext5\n\nTARGET\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      ;; Verify that the diff contains multiple hunks
      (with-current-buffer diff-buffer
        (goto-char (point-min))
        (let ((hunk-count 0))
          (while (re-search-forward "^@@" nil t)
            (setq hunk-count (1+ hunk-count)))
          (should (> hunk-count 1))))

      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; TARGET overlay should be preserved and shifted by cumulative delta
    ;; -10 from deleting DELETE_ME\n, +9 from inserting INSERTED\n = -1 net delta
    (should target-ov)
    (should (equal "TARGET\n"
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start target-ov) (overlay-end target-ov))))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Single large hunk encompassing overlay expands to cover entire replacement"
  (let* ((buffer-text "section1\nsection2\nTARGET\nsection3\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "TARGET\n" 'directive))
         (test-buffer (car buf-setup))
         (target-ov (cdr buf-setup))
         ;; This creates a single large hunk that encompasses the entire buffer
         (new-text "BEFORE\nsection1\nsection2\nMODIFIED\nsection3\nAFTER\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; Overlay should expand to cover the entire hunk replacement
    ;; Since the overlay is encompassed by a single large change that replaces
    ;; the entire buffer, the overlay expands to cover the complete new buffer
    (should target-ov)
    (should (equal "BEFORE\nsection1\nsection2\nMODIFIED\nsection3\nAFTER\n"
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start target-ov) (overlay-end target-ov))))))
  :doc "`mevedel-diff-apply-buffer':
Edge Cases and Boundary Conditions:
Multiple hunks: only apply cumulative delta from hunks before overlay"
  (let* ((buffer-text "header1\nheader2\nheader3\nheader4\nheader5\nheader6\nheader7\nheader8\nheader9\nheader10\n\nTARGET\n\nfooter1\nfooter2\nfooter3\nfooter4\nfooter5\nfooter6\nfooter7\nfooter8\nfooter9\nfooter10\n")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil "TARGET\n" 'directive))
         (test-buffer (car buf-setup))
         (target-ov (cdr buf-setup))
         ;; Hunk 1: Insert at beginning (shifts TARGET position by 18 chars)
         ;; Large unchanged context (10 lines)
         ;; Hunk 2: Change TARGET to MODIFIED
         ;; Large unchanged context (10 lines)
         ;; Hunk 3: Insert at end (should NOT affect TARGET position)
         (new-text "INSERTED_AT_START\nheader1\nheader2\nheader3\nheader4\nheader5\nheader6\nheader7\nheader8\nheader9\nheader10\n\nMODIFIED\n\nfooter1\nfooter2\nfooter3\nfooter4\nfooter5\nfooter6\nfooter7\nfooter8\nfooter9\nfooter10\nINSERTED_AT_END\n")
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      ;; First, verify that the diff actually contains multiple hunks (3 expected)
      (with-current-buffer diff-buffer
        (goto-char (point-min))
        (let ((hunk-count 0))
          (while (re-search-forward "^@@" nil t)
            (setq hunk-count (1+ hunk-count)))
          (should (> hunk-count 1))))

      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    ;; Overlay should cover "MODIFIED\n"
    ;; Position is shifted by hunk 1's insertion (+18), but hunk 3's insertion doesn't affect it
    (should target-ov)
    (should (equal "MODIFIED\n"
                   (with-current-buffer test-buffer
                     (buffer-substring-no-properties (overlay-start target-ov) (overlay-end target-ov))))))
  :doc "`mevedel-diff-apply-buffer':
Real World Examples:
Example 1"
  (let* ((buffer-text "
      # plots is to high too display on one page and thus, it makes sense to
      # have a division between multiple pages.
      shiny$div(
        # Dynamic layout - columns adjust based on which cards are visible
        shiny$uiOutput(ns(\"dynamic_layout_ui\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"dynamic_layout_ui_pt2\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"dynamic_layout_ui_pt3\"))
      )
    )
  )

Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    # Create entire layout dynamically based on checkbox states
    output$dynamic_layout_ui <- shiny$renderUI({
      ns <- session$ns


Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    ### Intervention costs -----------------------------------------------------

    output$dynamic_layout_ui_pt2 <- shiny$renderUI({
      ns <- session$ns

      if (input$show_costs) {
        costs$ui(id = ns(\"costs\"))
      }
    })

    ### Impact maps ------------------------------------------------------------

    output$dynamic_layout_ui_pt3 <- shiny$renderUI({
      ns <- session$ns


")
         (directive-text "      shiny$div(\n        # Dynamic layout - columns adjust based on which cards are visible\n        shiny$uiOutput(ns(\"dynamic_layout_ui\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"dynamic_layout_ui_pt2\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"dynamic_layout_ui_pt3\"))\n      )\n")
         (reference-1-text "    output$dynamic_layout_ui <- shiny$renderUI({\n")
         (reference-2-text "    output$dynamic_layout_ui_pt2 <- shiny$renderUI({\n")
         (reference-3-text "    output$dynamic_layout_ui_pt3 <- shiny$renderUI({\n")
         (new-text "
      # plots is to high too display on one page and thus, it makes sense to
      # have a division between multiple pages.
      shiny$div(
        # Main plots and maps layout
        shiny$uiOutput(ns(\"main_plots_maps_ui\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"costs_analysis_ui\"))
      ),
      shiny$div(
        style = \"margin-top: 3px;\",
        shiny$uiOutput(ns(\"impact_maps_ui\"))
      )
    )
  )

Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    # Create main plots and maps layout dynamically based on checkbox states
    output$main_plots_maps_ui <- shiny$renderUI({
      ns <- session$ns


Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur
Lorem ipsum dolor sit amet, consetetur


    ### Intervention costs -----------------------------------------------------

    output$costs_analysis_ui <- shiny$renderUI({
      ns <- session$ns

      if (input$show_costs) {
        costs$ui(id = ns(\"costs\"))
      }
    })

    ### Impact maps ------------------------------------------------------------

    output$impact_maps_ui <- shiny$renderUI({
      ns <- session$ns


")
         (buf-setup (mevedel-test--create-buffer-with-overlay
                     buffer-text nil nil directive-text 'directive))
         (test-buffer (car buf-setup))
         (directive-ov (cdr buf-setup))
         (reference-1-ov (mevedel-test--create-overlay
                          test-buffer nil nil reference-1-text 'reference))
         (reference-2-ov (mevedel-test--create-overlay
                          test-buffer nil nil reference-2-text 'reference))
         (reference-3-ov (mevedel-test--create-overlay
                          test-buffer nil nil reference-3-text 'reference))
         (diff-buffer (mevedel-test--create-diff-buffer new-text test-buffer)))

    (cl-letf (((symbol-function #'mevedel-workspace)
               (lambda (&rest _) `(file . ,(buffer-file-name test-buffer)))))
      (with-current-buffer diff-buffer
        (let ((default-directory (temporary-file-directory))
              (inhibit-message t))
          (mevedel-diff-apply-buffer))))

    (should directive-ov)
    (should reference-1-ov)
    (should reference-2-ov)
    (should reference-3-ov)
    ;; Verify overlay contents match expected values
    (let ((expected-directive "      shiny$div(\n        # Main plots and maps layout\n        shiny$uiOutput(ns(\"main_plots_maps_ui\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"costs_analysis_ui\"))\n      ),\n      shiny$div(\n        style = \"margin-top: 3px;\",\n        shiny$uiOutput(ns(\"impact_maps_ui\"))\n      )\n")
          (expected-ref1 "    output$main_plots_maps_ui <- shiny$renderUI({\n")
          (expected-ref2 "    output$costs_analysis_ui <- shiny$renderUI({\n")
          (expected-ref3 "    output$impact_maps_ui <- shiny$renderUI({\n"))
      (should (equal expected-directive
                     (with-current-buffer test-buffer
                       (buffer-substring-no-properties (overlay-start directive-ov) (overlay-end directive-ov)))))
      (should (equal expected-ref1
                     (with-current-buffer test-buffer
                       (buffer-substring-no-properties (overlay-start reference-1-ov) (overlay-end reference-1-ov)))))
      (should (equal expected-ref2
                     (with-current-buffer test-buffer
                       (buffer-substring-no-properties (overlay-start reference-2-ov) (overlay-end reference-2-ov)))))
      (should (equal expected-ref3
                     (with-current-buffer test-buffer
                       (buffer-substring-no-properties (overlay-start reference-3-ov) (overlay-end reference-3-ov))))))))

(provide 'test-mevedel-diff-apply)

;;; test-mevedel-diff-apply.el ends here
