;;; test-niwa-view-background-cursor.el --- Background reader regression probes -*- lexical-binding: t -*-

;;; Commentary:
;; Preserve actual background header and agent-row positions without an Ask
;; card or an explicit interaction visit supplying extra restoration state.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agent-control)
(require 'niwa-tool-exec)
(require 'niwa-tool-introspect)
(require 'niwa-view)
(require 'niwa-view-agent)
(require 'niwa-view-composer)
(require 'niwa-view-render)
(require 'niwa-view-stream)

(niwa-deftest niwa-view-background-cursor ()
  (dolist (label '("[Manage]" "/root/background-reader"))
    (niwa-view-test--with-buffers
      (save-window-excursion
        (let* ((session (niwa-session--create
                         :name "background-cursor" :root-buffer data-buf))
               (record (niwa-agent-record--create
                        :id "background-reader" :path "/root/background-reader"
                        :parent-path "/root" :activity 'running))
               (niwa-view-spinner-animate nil)
               (niwa-view-stream-render-delay 0))
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
          (niwa-view-test--insert-data data-buf "Initial assistant response.\n" 'response)
          (niwa-view-render-live-update data-buf)
          (niwa-view--render-status)
          (niwa-view--start-spinner)
          (niwa-view-test--insert-composer-draft "> unsent draft\nsecond line" 4)
          (goto-char (point-min))
          (search-forward label)
          (backward-char (length label))
          (should-not niwa-view--interaction-focus-return)
          (let ((anchor (niwa-view-zone-position-anchor (point))))
            (should anchor)
            ,action
            (ert-info ((format "background label=%S point=%S anchor=%S text=%S"
                               label (point) anchor
                               (buffer-substring-no-properties
                                (point) (min (point-max) (+ (point) 60)))))
              (should (looking-at-p (regexp-quote label)))
              (should (= (point) (niwa-view-zone-anchor-position anchor)))
              (should (= (point) (window-point (selected-window))))
              (should-not niwa-view--interaction-focus-return)
              (should (equal "> unsent draft\nsecond line"
                             (niwa-view-composer-draft-text)))))))))
  (action)
  :doc "full redraw preserves the background header and actual agent row"
  (niwa-view--full-rerender)
  :doc "stream growth preserves the background header and actual agent row"
  (progn
    (niwa-view-test--insert-data data-buf "New streamed response text.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-schedule))
    (should (string-search "New streamed response text." (buffer-string))))
  :doc "terminal response preserves the background header and actual agent row"
  (progn
    (niwa-view-test--insert-data data-buf "Final response text.\n" 'response)
    (with-current-buffer data-buf
      (niwa-view-stream-render-response (point-min) (point-max)))
    (should (string-search "Final response text." (buffer-string)))
    (should-not niwa-view--in-flight-turn-start))
  :doc "stream stop preserves the background header and actual agent row"
  (niwa-view-stream-stop)
  :doc "live Eval restoration does not return a background reader to Assistant"
  (let ((expression (format "(with-current-buffer %S (niwa-view--full-rerender))"
                            (buffer-name view-buf)))
        result)
    ;; Production tool dispatch evaluates with the data buffer current while
    ;; the selected window shows the view.  Restoring a raw window configuration
    ;; here used to override the renderer's already-restored background anchor.
    (with-current-buffer data-buf
      (niwa-tool-exec--eval-live
       (lambda (response) (setq result (plist-get response :result)))
       expression "injection" t))
    (should (stringp result))
    (should-not (string-prefix-p "Error:" result)))
  :doc "Info navigation does not overwrite a background anchor restored by redraw"
  (let ((window (selected-window))
        (windows (window-list))
        (before (buffer-list))
        (Info-history nil)
        (Info-history-list nil)
        (hits 0)
        (restored 0))
    (unwind-protect
        (let ((Info-selection-hook
               (list (lambda ()
                       (cl-incf hits)
                       (with-current-buffer view-buf
                         (niwa-view--full-rerender)
                         (should (looking-at-p (regexp-quote label)))
                         (should (= (point)
                                    (niwa-view-zone-anchor-position anchor)))
                         (cl-incf restored))))))
          (with-current-buffer data-buf
            (should (string-match-p
                     "Node: Top"
                     (niwa-tool-introspect--manual-node-contents "elisp" "Top")))
            (should (eq (current-buffer) data-buf)))
          (should (> hits 0))
          (should (= hits restored))
          (should (eq (selected-window) window))
          (should (equal (window-list) windows))
          (should (eq (window-buffer window) view-buf)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))

(niwa-deftest niwa-view-call-preserving-window-configuration ()
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "draft survives" 3)
      (let ((window (selected-window))
            (windows (window-list))
            (draft (niwa-view-composer-draft-text)))
        (with-current-buffer data-buf
          ,action
          (should (eq (current-buffer) data-buf)))
        (should (eq (selected-window) window))
        (should (equal (window-list) windows))
        (should (eq (window-buffer window) view-buf))
        (should (= 3 (with-current-buffer (niwa-view-composer-buffer view-buf)
                       (- (point) (niwa-view-composer-draft-start view-buf)))))
        (should (equal draft (niwa-view-composer-draft-text)))
        (should (with-current-buffer data-buf
                  (string-search "intentional edit" (buffer-string)))))))
  (action)
  :doc "returns thunk value and restores layout without reverting buffer edits"
  (should
   (eq 'returned
       (niwa-view-call-preserving-window-configuration
        (lambda ()
          (should (eq (current-buffer) data-buf))
          (goto-char (point-max))
          (insert "intentional edit")
          (with-current-buffer view-buf (niwa-view--full-rerender))
          (split-window-right)
          (switch-to-buffer data-buf)
          'returned))))
  :doc "errors restore layout and semantic point without reverting edits"
  (should
   (equal
    '(error "Expected background cursor probe error")
    (should-error
     (niwa-view-call-preserving-window-configuration
      (lambda ()
        (should (eq (current-buffer) data-buf))
        (goto-char (point-max))
        (insert "intentional edit")
        (with-current-buffer view-buf (niwa-view--full-rerender))
        (split-window-right)
        (switch-to-buffer data-buf)
        (error "Expected background cursor probe error")))
     :type 'error))))

(ert-deftest niwa-view-window-configuration-killed-view ()
  "Killing a preserved view does not restore its point in another buffer."
  (niwa-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "a long draft in a doomed view" 10)
      ;; View teardown also kills its data buffer; the unrelated caller must
      ;; really be independent of that pair rather than bypassing kill hooks.
      (with-temp-buffer
        (let ((caller (current-buffer)))
          (insert "unrelated buffer contents")
          (should
           (eq 'returned
               (niwa-view-call-preserving-window-configuration
                (lambda ()
                  (kill-buffer view-buf)
                  (set-buffer caller)
                  (goto-char 2)
                  'returned))))
          (should (eq caller (current-buffer)))
          (should (= 2 (point)))))
      (should-not (buffer-live-p view-buf)))))

(ert-deftest niwa-view-window-configuration-multiple-views ()
  "Restore distinct view drafts and window points with a data buffer current."
  (niwa-view-test--with-buffers
    (let ((first-view view-buf)
          (first-data data-buf))
      (niwa-view-test--with-buffers
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer first-view)
          (niwa-view-test--insert-composer-draft "first view draft" 2)
          (let* ((first-window (selected-window))
                 (second-window (split-window-right))
                 (first-composer (niwa-view-composer-display first-window)))
            (should (window-live-p first-composer))
            (select-window second-window)
            (switch-to-buffer view-buf)
            (niwa-view-test--insert-composer-draft "second view draft" 5)
            (let ((second-composer (niwa-view-composer-display second-window)))
              (should (window-live-p second-composer))
              (with-current-buffer first-data
                (should
                 (eq 'returned
                     (niwa-view-call-preserving-window-configuration
                      (lambda ()
                        (should (eq (current-buffer) first-data))
                        (with-current-buffer first-view (niwa-view--full-rerender))
                        (with-current-buffer view-buf (niwa-view--full-rerender))
                        (delete-other-windows)
                        (switch-to-buffer first-data)
                        'returned))))
                (should (eq (current-buffer) first-data)))
              (should (eq (selected-window) second-window))
              (should (eq (window-buffer first-window) first-view))
              (should (eq (window-buffer second-window) view-buf))
              (should (eq first-composer (niwa-view-composer-window first-window)))
              (should (eq second-composer (niwa-view-composer-window second-window)))
              (should (equal "first view draft" (niwa-view-composer-draft-text first-view)))
              (should (equal "second view draft" (niwa-view-composer-draft-text view-buf)))
              (should (= (window-point first-composer)
                         (+ (niwa-view-composer-draft-start first-view) 2)))
              (should (= (window-point second-composer)
                         (+ (niwa-view-composer-draft-start view-buf) 5))))))))))

(ert-deftest niwa-view-window-configuration-same-view-two-windows ()
  "One view shown twice retains each window's distinct composer offset."
  (niwa-view-test--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (niwa-view-test--insert-composer-draft "shared view draft" 2)
      (let* ((first-window (selected-window))
             (second-window (split-window-right))
             (first-composer (niwa-view-composer-display first-window))
             (second-composer (niwa-view-composer-display second-window))
             (start (niwa-view-composer-draft-start)))
        (should (and (window-live-p first-composer) (window-live-p second-composer)))
        (should-not (eq first-composer second-composer))
        (set-window-point second-composer (+ start 7))
        (with-current-buffer data-buf
          (niwa-view-call-preserving-window-configuration
           (lambda ()
             (with-current-buffer view-buf (niwa-view--full-rerender))
             (select-window second-window)
             (delete-other-windows)
             (switch-to-buffer data-buf))))
        (should (eq (selected-window) first-window))
        (should (eq (window-buffer first-window) view-buf))
        (should (eq (window-buffer second-window) view-buf))
        (should (eq first-composer (niwa-view-composer-window first-window)))
        (should (eq second-composer (niwa-view-composer-window second-window)))
        (should (equal "shared view draft" (niwa-view-composer-draft-text)))
        (should (= (window-point first-composer) (+ start 2)))
        (should (= (window-point second-composer) (+ start 7)))))))

(provide 'test-niwa-view-background-cursor)
;;; test-niwa-view-background-cursor.el ends here
