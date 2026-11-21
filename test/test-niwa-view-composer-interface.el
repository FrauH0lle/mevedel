;;; test-niwa-view-composer-interface.el -- Composer draft access tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise composer access through a real chat view.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-composer)

(ert-deftest niwa-view-composer-buffer/owned-lifetime ()
  "Each editable view owns a distinct composer for its lifetime."
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (should-not (niwa-view-composer-buffer))
      (let ((composer (niwa-view-composer-buffer view-buf)))
        (should (buffer-live-p composer))
        (should-not (memq composer (list view-buf data-buf)))
        (should (eq composer (niwa-view-composer-buffer view-buf)))
        (should (eq data-buf (current-buffer)))
        (kill-buffer view-buf)
        (should-not (buffer-live-p composer))))))

(ert-deftest niwa-composer-mode/refuses-adaptive-wrap ()
  "A `text-mode-hook' enabling adaptive wrap leaves the composer's prefixes alone."
  (unless (fboundp 'adaptive-wrap-prefix-mode)
    (define-minor-mode adaptive-wrap-prefix-mode "Stand-in." :lighter nil))
  (let ((text-mode-hook (list #'adaptive-wrap-prefix-mode)))
    (with-temp-buffer
      (niwa-composer-mode)
      (should-not (bound-and-true-p adaptive-wrap-prefix-mode)))))

(niwa-deftest niwa-view-composer-buffer ()
  ,test
  (test)
  :doc "A read-only agent transcript has no composer even with zone markers"
  (niwa-view-test--with-buffers
    (niwa-view--setup view-buf data-buf '(:agent-transcript-p t))
    (should-not (niwa-view-composer-buffer view-buf))
    (should-not (niwa-view-composer-draft-text view-buf))))

(niwa-deftest niwa-view-composer-window ()
  ,test
  (test)
  :doc "Resolve the composer relative to a particular window without selecting it"
  (niwa-view-test--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window))
            (view-window (split-window)))
        (set-window-buffer origin data-buf)
        (set-window-buffer view-window view-buf)
        (should-not (niwa-view-composer-window))
        (should-not (niwa-view-composer-window view-window))
        (let ((composer-window (niwa-view-composer-display view-window)))
          (should (window-live-p composer-window))
          (should-not (eq composer-window view-window))
          (should (eq composer-window (niwa-view-composer-window view-window)))
          (should (eq (window-buffer composer-window)
                      (niwa-view-composer-buffer view-buf))))
        (should (eq origin (selected-window)))))))

(niwa-deftest niwa-view-composer-in-draft-p ()
  ,test
  (test)
  :doc "An empty draft has one valid caret position, after the read-only prompt"
  (niwa-view-test--with-buffers
    (let ((start (niwa-view-composer-draft-start view-buf))
          (end (niwa-view-composer-draft-end view-buf)))
      (should (= start end))
      (with-current-buffer (niwa-view-composer-buffer view-buf)
        (should (equal (buffer-substring-no-properties (- start 2) start) "> "))
        (should (niwa-view-composer-in-draft-p start view-buf))
        (should (niwa-view-composer-in-draft-p (copy-marker start) view-buf))
        (should-not (niwa-view-composer-in-draft-p (1- start) view-buf))
        (should-not (niwa-view-composer-in-draft-p (1+ end) view-buf)))
      (with-temp-buffer
        (insert (make-string (1- start) ?x))
        (should-not (niwa-view-composer-in-draft-p (point-marker) view-buf))))))

(niwa-deftest niwa-view-composer-draft-text ()
  ,test
  (test)
  :doc "Read the full untrimmed draft, with or without its text properties"
  (niwa-view-test--with-buffers
    (should (equal "" (niwa-view-composer-draft-text view-buf)))
    (let ((start (niwa-view-composer-draft-start view-buf)))
      (niwa-view-composer-insert (propertize "> hello\nthere " 'face 'bold)
                                    start view-buf)
      (with-current-buffer (niwa-view-composer-buffer view-buf)
        (save-restriction
          (narrow-to-region (point-min) start)
          (let ((text (niwa-view-composer-draft-text view-buf))
                (plain (niwa-view-composer-draft-text view-buf t)))
            (should (equal "> hello\nthere " text))
            (should (eq 'bold (get-text-property 0 'face text)))
            (should (equal-including-properties "> hello\nthere " plain)))
          (should (= (point-max) start)))))))

(niwa-deftest niwa-view-composer-insert ()
  ,test
  (test)
  :doc "Insert at the requested draft position or append, never editing the prompt"
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (niwa-view-composer-draft-start view-buf)))
        (should (= (+ start 5) (niwa-view-composer-insert "tail " nil view-buf)))
        (should (= (+ start 2) (niwa-view-composer-insert "> " start view-buf)))
        (should-error (niwa-view-composer-insert "outside" (1- start) view-buf))
        (should (eq data-buf (current-buffer)))
        (should (equal "> tail " (niwa-view-composer-draft-text view-buf t)))
        (with-current-buffer (niwa-view-composer-buffer view-buf)
          (should (= (point) (+ start 2)))
          (should (equal "> " (buffer-substring-no-properties (- start 2) start))))))))

(niwa-deftest niwa-view-composer-replace ()
  ,test
  (test test)
  :doc "Replace or clear the whole draft without changing the transcript or prompt"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((start (niwa-view-composer-draft-start))
             (transcript (buffer-substring (point-min) (niwa-view-transcript-tail)))
             (prefix (with-current-buffer (niwa-view-composer-buffer)
                       (buffer-substring (point-min) start))))
        (niwa-view-composer-insert "old\nlong draft")
        (should (= (+ start 3) (niwa-view-composer-replace "new")))
        (should (= 3 (niwa-view-test--composer-point-offset)))
        (should (equal "new" (niwa-view-composer-draft-text)))
        (should-error (niwa-view-composer-replace '(not text)))
        (should (equal "new" (niwa-view-composer-draft-text)))
        (should (= start (niwa-view-composer-replace "")))
        (should (equal "" (niwa-view-composer-draft-text)))
        (should (equal-including-properties
                 transcript (buffer-substring (point-min) (niwa-view-transcript-tail))))
        (with-current-buffer (niwa-view-composer-buffer)
          (should (equal-including-properties
                   prefix (buffer-substring (point-min) start)))))))
  :doc "A hidden, locked composer retains its draft and rejects edits"
  (niwa-view-test--with-buffers
    (with-current-buffer view-buf
      (niwa-view-composer-insert "keep me")
      (niwa-view-composer-set-historical-visible nil)
      (should-error (niwa-view-composer-insert "no") :type 'buffer-read-only)
      (should-error (niwa-view-composer-replace "no") :type 'buffer-read-only)
      (should (equal "keep me" (niwa-view-composer-draft-text))))))

(provide 'test-niwa-view-composer-interface)
;;; test-niwa-view-composer-interface.el ends here
