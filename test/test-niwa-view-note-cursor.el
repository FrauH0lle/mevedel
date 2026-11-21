;;; test-niwa-view-note-cursor.el --- Note input cursor tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise region note creation through its real minibuffer while a
;; neighboring chat redraws, including normal acceptance and unwinding.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-overlays)
(require 'niwa-notes)
(require 'niwa-agent-control)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-render)
(require 'niwa-view-stream)

(niwa-deftest niwa-view-note-cursor
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)))
  (dolist (label '("[Manage]" "/root/background-reader"))
    (niwa-view-test--with-buffers
      (let* ((directory (make-temp-file "niwa-note-cursor-" t))
             (file (file-name-concat directory "source.txt"))
             (workspace (niwa-workspace--create
                         :type 'file :id directory :root directory :name "cursor"))
             source)
        (unwind-protect
            (save-window-excursion
              (make-directory (file-name-concat directory ".niwa"))
              (with-temp-file file
                (dotimes (_ 80) (insert "Selected source text\n")))
              (setq source (find-file-noselect file))
              (with-current-buffer source (setq-local niwa--workspace workspace))
              (let* ((session (niwa-session--create
                               :name "note-cursor" :root-buffer data-buf))
                     (record (niwa-agent-record--create
                              :id "background-reader" :path "/root/background-reader"
                              :parent-path "/root" :activity 'running))
                     (niwa-view-spinner-animate nil))
                (setf (niwa-session-agent-registry session)
                      (list (cons "/root/background-reader" record)))
                (with-current-buffer data-buf
                  (setq-local niwa--session session)
                  (insert "*** Inspect running work\n"))
                (switch-to-buffer view-buf)
                (setq niwa-view--data-turn-start
                      (with-current-buffer data-buf (copy-marker (point-min)))
                      niwa-view--in-flight-turn-start
                      (copy-marker niwa-view--status-marker))
                (niwa-view-test--insert-data data-buf "Assistant response.\n" 'response)
                (niwa-view-render-live-update data-buf)
                (niwa-view--render-status)
                (niwa-view--start-spinner)
                (niwa-view-test--insert-composer-draft "Unsent draft\nsecond line" 3)
                (goto-char (point-min))
                (search-forward label)
                (backward-char (length label))
                (let* ((anchor (niwa-view-zone-position-anchor (point)))
                       (view-window (selected-window))
                       (source-window (split-window-below))
                       (windows (window-list))
                       (outcome ,outcome)
                       (hits 0)
                       (restored 0)
                       scroll-start
                       note result)
                  (should anchor)
                  (select-window source-window)
                  (switch-to-buffer source)
                  (goto-char (point-min))
                  (push-mark 9 t t)
                  (let* ((transient-mark-mode t)
                         (input-map (make-sparse-keymap))
                         (minibuffer-setup-hook
                          (cons
                           (lambda ()
                             (should (minibufferp))
                             (should (= 1 (minibuffer-depth)))
                             (should (equal "Note (optional): " (minibuffer-prompt)))
                             (should (= scroll-start (window-start source-window)))
                             (cl-incf hits)
                             (insert "Preserve this source")
                             (with-current-buffer view-buf
                               (niwa-view--full-rerender)
                               (should (looking-at-p (regexp-quote label)))
                               (should (= (window-point view-window)
                                          (niwa-view-zone-anchor-position anchor)))
                               (cl-incf restored))
                             (when (eq outcome 'error)
                               (error "Expected note input failure")))
                           minibuffer-setup-hook)))
                    (define-key input-map (kbd "C-c d")
                                (lambda ()
                                  (interactive)
                                  (setq scroll-start (window-start source-window))
                                  (setq result
                                        (condition-case err
                                            (progn
                                              (setq note
                                                    (call-interactively #'niwa-note-create))
                                              'accepted)
                                          (quit 'cancelled)
                                          (error err)))))
                    (use-local-map input-map)
                    ;; A keyboard macro enters the real minibuffer even in
                    ;; batch Emacs; read-from-minibuffer itself is not mocked.
                    (execute-kbd-macro
                     (kbd (pcase outcome
                            ('quit "C-c d C-g")
                            ('error "C-c d")
                            (_ "C-c d RET")))))
                  (should (= hits 1))
                  (ert-info ((format "Note result: %S" result))
                    (should (= restored 1)))
                  (should (= (minibuffer-depth) 0))
                  (should-not (active-minibuffer-window))
                  (pcase outcome
                    ('quit
                     (should (eq result 'cancelled))
                     (should-not (niwa--instructions-in (point-min) (point-max))))
                    ('error
                     (should (equal result '(error "Expected note input failure"))))
                    (_
                     (should (eq result 'accepted))
                     (should (overlay-buffer note))
                     (should (equal "Preserve this source"
                                    (overlay-get note 'niwa-note-text)))))
                  (should (eq (current-buffer) source))
                  (should (eq (selected-window) source-window))
                  (should (equal (window-list nil nil view-window) windows))
                  (should (eq (window-buffer view-window) view-buf))
                  (should (eq (window-buffer source-window) source))
                  (should (= (window-start source-window) scroll-start))
                  (with-current-buffer view-buf
                    (ert-info ((format "outcome=%S label=%S" outcome label))
                      (should (looking-at-p (regexp-quote label)))
                      (should (= (window-point view-window)
                                 (niwa-view-zone-anchor-position anchor)))
                      (should (equal "Unsent draft\nsecond line"
                                     (niwa-view-composer-draft-text))))))))
          (when (buffer-live-p source)
            (with-current-buffer source
              (setq-local kill-buffer-hook nil)
              (set-buffer-modified-p nil))
            (kill-buffer source))
          (delete-directory directory t)))))
  (outcome)
  :doc "accepting note input preserves a neighboring background reader"
  'accept
  :doc "quitting note input cleans up and preserves the background reader"
  'quit
  :doc "input errors unwind windows without losing the background reader"
  'error)

(provide 'test-niwa-view-note-cursor)
;;; test-niwa-view-note-cursor.el ends here
