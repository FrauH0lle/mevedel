;;; test-mevedel-view-composer-interface.el -- Composer draft access tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise composer access through a real chat view.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view)
(require 'mevedel-view-composer)

(ert-deftest mevedel-view-composer-buffer/owned-lifetime ()
  "Each editable view owns a distinct composer for its lifetime."
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (should-not (mevedel-view-composer-buffer))
      (let ((composer (mevedel-view-composer-buffer view-buf)))
        (should (buffer-live-p composer))
        (should-not (memq composer (list view-buf data-buf)))
        (should (eq composer (mevedel-view-composer-buffer view-buf)))
        (should (eq data-buf (current-buffer)))
        (kill-buffer view-buf)
        (should-not (buffer-live-p composer))))))

(ert-deftest mevedel-composer-mode/refuses-adaptive-wrap ()
  "A `text-mode-hook' enabling adaptive wrap leaves the composer's prefixes alone."
  (unless (fboundp 'adaptive-wrap-prefix-mode)
    (define-minor-mode adaptive-wrap-prefix-mode "Stand-in." :lighter nil))
  (let ((text-mode-hook (list #'adaptive-wrap-prefix-mode)))
    (with-temp-buffer
      (mevedel-composer-mode)
      (should-not (bound-and-true-p adaptive-wrap-prefix-mode)))))

(mevedel-deftest mevedel-view-composer-buffer ()
  ,test
  (test)
  :doc "A read-only agent transcript has no composer even with zone markers"
  (mevedel-view-test--with-buffers
    (mevedel-view--setup view-buf data-buf '(:agent-transcript-p t))
    (should-not (mevedel-view-composer-buffer view-buf))
    (should-not (mevedel-view-composer-draft-text view-buf))))

(mevedel-deftest mevedel-view-composer-window ()
  ,test
  (test)
  :doc "Resolve the composer relative to a particular window without selecting it"
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window))
            (view-window (split-window)))
        (set-window-buffer origin data-buf)
        (set-window-buffer view-window view-buf)
        (should-not (mevedel-view-composer-window))
        (should-not (mevedel-view-composer-window view-window))
        (let ((composer-window (mevedel-view-composer-display view-window)))
          (should (window-live-p composer-window))
          (should-not (eq composer-window view-window))
          (should (eq composer-window (mevedel-view-composer-window view-window)))
          (should (eq (window-buffer composer-window)
                      (mevedel-view-composer-buffer view-buf))))
        (should (eq origin (selected-window)))))))

(mevedel-deftest mevedel-view-composer-in-draft-p ()
  ,test
  (test)
  :doc "An empty draft has one valid caret position, after the read-only prompt"
  (mevedel-view-test--with-buffers
    (let ((start (mevedel-view-composer-draft-start view-buf))
          (end (mevedel-view-composer-draft-end view-buf)))
      (should (= start end))
      (with-current-buffer (mevedel-view-composer-buffer view-buf)
        (should (equal (buffer-substring-no-properties (- start 2) start) "> "))
        (should (mevedel-view-composer-in-draft-p start view-buf))
        (should (mevedel-view-composer-in-draft-p (copy-marker start) view-buf))
        (should-not (mevedel-view-composer-in-draft-p (1- start) view-buf))
        (should-not (mevedel-view-composer-in-draft-p (1+ end) view-buf)))
      (with-temp-buffer
        (insert (make-string (1- start) ?x))
        (should-not (mevedel-view-composer-in-draft-p (point-marker) view-buf))))))

(mevedel-deftest mevedel-view-composer-draft-text ()
  ,test
  (test)
  :doc "Read the full untrimmed draft, with or without its text properties"
  (mevedel-view-test--with-buffers
    (should (equal "" (mevedel-view-composer-draft-text view-buf)))
    (let ((start (mevedel-view-composer-draft-start view-buf)))
      (mevedel-view-composer-insert (propertize "> hello\nthere " 'face 'bold)
                                    start view-buf)
      (with-current-buffer (mevedel-view-composer-buffer view-buf)
        (save-restriction
          (narrow-to-region (point-min) start)
          (let ((text (mevedel-view-composer-draft-text view-buf))
                (plain (mevedel-view-composer-draft-text view-buf t)))
            (should (equal "> hello\nthere " text))
            (should (eq 'bold (get-text-property 0 'face text)))
            (should (equal-including-properties "> hello\nthere " plain)))
          (should (= (point-max) start)))))))

(mevedel-deftest mevedel-view-composer-insert ()
  ,test
  (test)
  :doc "Insert at the requested draft position or append, never editing the prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (let ((start (mevedel-view-composer-draft-start view-buf)))
        (should (= (+ start 5) (mevedel-view-composer-insert "tail " nil view-buf)))
        (should (= (+ start 2) (mevedel-view-composer-insert "> " start view-buf)))
        (should-error (mevedel-view-composer-insert "outside" (1- start) view-buf))
        (should (eq data-buf (current-buffer)))
        (should (equal "> tail " (mevedel-view-composer-draft-text view-buf t)))
        (with-current-buffer (mevedel-view-composer-buffer view-buf)
          (should (= (point) (+ start 2)))
          (should (equal "> " (buffer-substring-no-properties (- start 2) start))))))))

(mevedel-deftest mevedel-view-composer-replace ()
  ,test
  (test test)
  :doc "Replace or clear the whole draft without changing the transcript or prompt"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (let* ((start (mevedel-view-composer-draft-start))
             (transcript (buffer-substring (point-min) (mevedel-view-transcript-tail)))
             (prefix (with-current-buffer (mevedel-view-composer-buffer)
                       (buffer-substring (point-min) start))))
        (mevedel-view-composer-insert "old\nlong draft")
        (should (= (+ start 3) (mevedel-view-composer-replace "new")))
        (should (= 3 (mevedel-view-test--composer-point-offset)))
        (should (equal "new" (mevedel-view-composer-draft-text)))
        (should-error (mevedel-view-composer-replace '(not text)))
        (should (equal "new" (mevedel-view-composer-draft-text)))
        (should (= start (mevedel-view-composer-replace "")))
        (should (equal "" (mevedel-view-composer-draft-text)))
        (should (equal-including-properties
                 transcript (buffer-substring (point-min) (mevedel-view-transcript-tail))))
        (with-current-buffer (mevedel-view-composer-buffer)
          (should (equal-including-properties
                   prefix (buffer-substring (point-min) start)))))))
  :doc "A hidden, locked composer retains its draft and rejects edits"
  (mevedel-view-test--with-buffers
    (with-current-buffer view-buf
      (mevedel-view-composer-insert "keep me")
      (mevedel-view-composer-set-historical-visible nil)
      (should-error (mevedel-view-composer-insert "no") :type 'buffer-read-only)
      (should-error (mevedel-view-composer-replace "no") :type 'buffer-read-only)
      (should (equal "keep me" (mevedel-view-composer-draft-text))))))

(provide 'test-mevedel-view-composer-interface)
;;; test-mevedel-view-composer-interface.el ends here
