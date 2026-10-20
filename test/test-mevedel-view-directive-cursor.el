;;; test-mevedel-view-directive-cursor.el --- Directive input cursor tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise region directive creation through its real minibuffer while a
;; neighboring chat redraws, including normal acceptance and unwinding.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-overlays)
(require 'mevedel-overlay-ui)
(require 'mevedel-agent-control)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(mevedel-deftest mevedel-view-directive-cursor
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)))
  (dolist (label '("[Manage]" "/root/background-reader"))
    (mevedel-view-test--with-buffers
      (let* ((directory (make-temp-file "mevedel-directive-cursor-" t))
             (file (file-name-concat directory "source.txt"))
             (workspace (mevedel-workspace--create
                         :type 'file :id directory :root directory :name "cursor"))
             source)
        (unwind-protect
            (save-window-excursion
              (with-temp-file file
                (dotimes (_ 80) (insert "Selected source text\n")))
              (setq source (find-file-noselect file))
              (with-current-buffer source (setq-local mevedel--workspace workspace))
              (let* ((session (mevedel-session--create
                               :name "directive-cursor" :root-buffer data-buf))
                     (record (mevedel-agent-record--create
                              :id "background-reader" :path "/root/background-reader"
                              :parent-path "/root" :activity 'running))
                     (mevedel-view-spinner-animate nil))
                (setf (mevedel-session-agent-registry session)
                      (list (cons "/root/background-reader" record)))
                (with-current-buffer data-buf
                  (setq-local mevedel--session session)
                  (insert "*** Inspect running work\n"))
                (switch-to-buffer view-buf)
                (setq mevedel-view--data-turn-start
                      (with-current-buffer data-buf (copy-marker (point-min)))
                      mevedel-view--in-flight-turn-start
                      (copy-marker mevedel-view--status-marker))
                (mevedel-view-test--insert-data data-buf "Assistant response.\n" 'response)
                (mevedel-view-render-live-update data-buf)
                (mevedel-view--render-status)
                (mevedel-view--start-spinner)
                (mevedel-view-test--insert-composer-draft "Unsent draft\nsecond line" 3)
                (goto-char (point-min))
                (search-forward label)
                (backward-char (length label))
                (let* ((anchor (mevedel-view-zone-position-anchor (point)))
                       (view-window (selected-window))
                       (source-window (split-window-below))
                       (windows (window-list))
                       (outcome ,outcome)
                       (hits 0)
                       (restored 0)
                       scroll-start
                       directive result)
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
                             (should (equal "Directive: " (minibuffer-prompt)))
                             (should (= (window-start source-window) 1))
                             (should (> scroll-start (window-start source-window)))
                             (cl-incf hits)
                             (insert "Preserve this source")
                             (with-current-buffer view-buf
                               (mevedel-view--full-rerender)
                               (should (looking-at-p (regexp-quote label)))
                               (should (= (window-point view-window)
                                          (mevedel-view-zone-anchor-position anchor)))
                               (cl-incf restored))
                             (when (eq outcome 'error)
                               (error "Expected directive input failure")))
                           minibuffer-setup-hook)))
                    (define-key input-map (kbd "C-c d")
                                (lambda ()
                                  (interactive)
                                  ;; Enter with the selected region above the
                                  ;; source viewport, exercising its temporary
                                  ;; scroll without a command-loop redisplay.
                                  (setq scroll-start
                                        (save-excursion
                                          (goto-char (point-min))
                                          (forward-line 40)
                                          (point)))
                                  (set-window-start source-window scroll-start t)
                                  (setq result
                                        (condition-case err
                                            (progn
                                              (setq directive
                                                    (mevedel--create-instruction 'directive))
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
                  (ert-info ((format "Directive result: %S" result))
                    (should (= restored 1)))
                  (should (= (minibuffer-depth) 0))
                  (should-not (active-minibuffer-window))
                  (pcase outcome
                    ('quit
                     (should (eq result 'cancelled))
                     (should-not (mevedel--instructions-in (point-min) (point-max)))
                     (should-not (mevedel-workspace-directives workspace)))
                    ('error
                     (should (equal result '(error "Expected directive input failure"))))
                    (_
                     (should (eq result 'accepted))
                     (should (overlay-buffer directive))
                     (should (equal "Preserve this source"
                                    (mevedel--directive-text directive)))))
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
                                 (mevedel-view-zone-anchor-position anchor)))
                      (should (equal "Unsent draft\nsecond line"
                                     (mevedel-view--input-text))))))))
          (when (buffer-live-p source)
            (with-current-buffer source
              (setq-local kill-buffer-hook nil)
              (set-buffer-modified-p nil))
            (kill-buffer source))
          (delete-directory directory t)))))
  (outcome)
  :doc "accepting directive input preserves a neighboring background reader"
  'accept
  :doc "quitting directive input cleans up and preserves the background reader"
  'quit
  :doc "input errors unwind windows without losing the background reader"
  'error)

(provide 'test-mevedel-view-directive-cursor)
;;; test-mevedel-view-directive-cursor.el ends here
