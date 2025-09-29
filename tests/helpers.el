;;; tests/helpers.el -- Helper functions for tests -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'mevedel-instructions)

(defun mevedel-tests--create-diff-buffer (modified &optional file-buffer original)
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

(defun mevedel-tests--create-overlay (buffer &optional start end text type-of)
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

(defun mevedel-tests--create-buffer-with-overlay (buf-text &optional ov-start ov-end ov-text type-of)
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
      (setq ov (mevedel-tests--create-overlay test-buffer ov-start ov-end ov-text type-of)))
    (cons test-buffer ov)))

(defun mevedel-tests--get-ov-at-point ()
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

(provide 'helpers)
;;; helpers.el ends here
