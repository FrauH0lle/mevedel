;;; test-mevedel-view-background-cursor.el --- Background reader regression probes -*- lexical-binding: t -*-

;;; Commentary:
;; Preserve actual background header and agent-row positions without an Ask
;; card or an explicit interaction visit supplying extra restoration state.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-introspect)
(require 'mevedel-view)
(require 'mevedel-view-agent)
(require 'mevedel-view-render)
(require 'mevedel-view-stream)

(mevedel-deftest mevedel-view-background-cursor ()
  (dolist (label '("[Manage]" "/root/background-reader"))
    (mevedel-view-test--with-buffers
      (save-window-excursion
        (let* ((session (mevedel-session--create
                         :name "background-cursor" :root-buffer data-buf))
               (record (mevedel-agent-record--create
                        :id "background-reader" :path "/root/background-reader"
                        :parent-path "/root" :activity 'running))
               (mevedel-view-spinner-animate nil)
               (mevedel-view-stream-render-delay 0))
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
          (mevedel-view-test--insert-data data-buf "Initial assistant response.\n" 'response)
          (mevedel-view-render-live-update data-buf)
          (mevedel-view--render-status)
          (mevedel-view--start-spinner)
          (mevedel-view-test--insert-composer-draft "> unsent draft\nsecond line" 4)
          (goto-char (point-min))
          (search-forward label)
          (backward-char (length label))
          (should-not mevedel-view--interaction-focus-return)
          (let ((anchor (mevedel-view-zone-position-anchor (point))))
            (should anchor)
            ,action
            (ert-info ((format "background label=%S point=%S anchor=%S text=%S"
                               label (point) anchor
                               (buffer-substring-no-properties
                                (point) (min (point-max) (+ (point) 60)))))
              (should (looking-at-p (regexp-quote label)))
              (should (= (point) (mevedel-view-zone-anchor-position anchor)))
              (should (= (point) (window-point (selected-window))))
              (should-not mevedel-view--interaction-focus-return)
              (should (equal "> unsent draft\nsecond line"
                             (mevedel-view--input-text)))))))))
  (action)
  :doc "full redraw preserves the background header and actual agent row"
  (mevedel-view--full-rerender)
  :doc "stream growth preserves the background header and actual agent row"
  (progn
    (mevedel-view-test--insert-data data-buf "New streamed response text.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-schedule))
    (should (string-search "New streamed response text." (buffer-string))))
  :doc "terminal response preserves the background header and actual agent row"
  (progn
    (mevedel-view-test--insert-data data-buf "Final response text.\n" 'response)
    (with-current-buffer data-buf
      (mevedel-view-stream-render-response (point-min) (point-max)))
    (should (string-search "Final response text." (buffer-string)))
    (should-not mevedel-view--in-flight-turn-start))
  :doc "stream stop preserves the background header and actual agent row"
  (mevedel-view-stream-stop)
  :doc "live Eval restoration does not return a background reader to Assistant"
  (let ((expression (format "(with-current-buffer %S (mevedel-view--full-rerender))"
                            (buffer-name view-buf)))
        result)
    ;; Production tool dispatch evaluates with the data buffer current while
    ;; the selected window shows the view.  Restoring a raw window configuration
    ;; here used to override the renderer's already-restored background anchor.
    (with-current-buffer data-buf
      (mevedel-tool-exec--eval-live
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
                         (mevedel-view--full-rerender)
                         (should (looking-at-p (regexp-quote label)))
                         (should (= (point)
                                    (mevedel-view-zone-anchor-position anchor)))
                         (cl-incf restored))))))
          (with-current-buffer data-buf
            (should (string-match-p
                     "Node: Top"
                     (mevedel-tool-introspect--manual-node-contents "elisp" "Top")))
            (should (eq (current-buffer) data-buf)))
          (should (> hits 0))
          (should (= hits restored))
          (should (eq (selected-window) window))
          (should (equal (window-list) windows))
          (should (eq (window-buffer window) view-buf)))
      (dolist (buffer (buffer-list))
        (unless (memq buffer before) (kill-buffer buffer))))))

(mevedel-deftest mevedel-view-call-preserving-window-configuration ()
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view-test--insert-composer-draft "draft survives" 3)
      (let ((window (selected-window))
            (windows (window-list))
            (draft (mevedel-view--input-text)))
        (with-current-buffer data-buf
          ,action
          (should (eq (current-buffer) data-buf)))
        (should (eq (selected-window) window))
        (should (equal (window-list) windows))
        (should (eq (window-buffer window) view-buf))
        (should (= 3 (- (point) (mevedel-view--input-start))))
        (should (equal draft (mevedel-view--input-text)))
        (should (with-current-buffer data-buf
                  (string-search "intentional edit" (buffer-string)))))))
  (action)
  :doc "returns thunk value and restores layout without reverting buffer edits"
  (should
   (eq 'returned
       (mevedel-view-call-preserving-window-configuration
        (lambda ()
          (should (eq (current-buffer) data-buf))
          (goto-char (point-max))
          (insert "intentional edit")
          (with-current-buffer view-buf (mevedel-view--full-rerender))
          (split-window-right)
          (switch-to-buffer data-buf)
          'returned))))
  :doc "errors restore layout and semantic point without reverting edits"
  (should
   (equal
    '(error "Expected background cursor probe error")
    (should-error
     (mevedel-view-call-preserving-window-configuration
      (lambda ()
        (should (eq (current-buffer) data-buf))
        (goto-char (point-max))
        (insert "intentional edit")
        (with-current-buffer view-buf (mevedel-view--full-rerender))
        (split-window-right)
        (switch-to-buffer data-buf)
        (error "Expected background cursor probe error")))
     :type 'error))))

(ert-deftest mevedel-view-window-configuration-killed-view ()
  "Killing a preserved view does not restore its point in another buffer."
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-view-test--insert-composer-draft "a long draft in a doomed view" 10)
      ;; View teardown also kills its data buffer; the unrelated caller must
      ;; really be independent of that pair rather than bypassing kill hooks.
      (with-temp-buffer
        (let ((caller (current-buffer)))
          (insert "unrelated buffer contents")
          (should
           (eq 'returned
               (mevedel-view-call-preserving-window-configuration
                (lambda ()
                  (kill-buffer view-buf)
                  (set-buffer caller)
                  (goto-char 2)
                  'returned))))
          (should (eq caller (current-buffer)))
          (should (= 2 (point)))))
      (should-not (buffer-live-p view-buf)))))

(ert-deftest mevedel-view-window-configuration-multiple-views ()
  "Restore distinct view drafts and window points with a data buffer current."
  (mevedel-view-test--with-buffers
    (let ((first-view view-buf)
          (first-data data-buf))
      (mevedel-view-test--with-buffers
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer first-view)
          (mevedel-view-test--insert-composer-draft "first view draft" 2)
          (let ((first-window (selected-window))
                (second-window (split-window-right)))
            (select-window second-window)
            (switch-to-buffer view-buf)
            (mevedel-view-test--insert-composer-draft "second view draft" 5)
            (with-current-buffer first-data
              (should
               (eq 'returned
                   (mevedel-view-call-preserving-window-configuration
                    (lambda ()
                      (should (eq (current-buffer) first-data))
                      (with-current-buffer first-view (mevedel-view--full-rerender))
                      (with-current-buffer view-buf (mevedel-view--full-rerender))
                      (delete-other-windows)
                      (switch-to-buffer first-data)
                      'returned))))
              (should (eq (current-buffer) first-data)))
            (should (eq (selected-window) second-window))
            (should (eq (window-buffer first-window) first-view))
            (should (eq (window-buffer second-window) view-buf))
            (with-current-buffer first-view
              (should (equal "first view draft" (mevedel-view--input-text)))
              (should (= 2 (- (window-point first-window) (mevedel-view--input-start)))))
            (with-current-buffer view-buf
              (should (equal "second view draft" (mevedel-view--input-text)))
              (should (= 5 (- (window-point second-window) (mevedel-view--input-start)))))))))))

(ert-deftest mevedel-view-window-configuration-same-view-two-windows ()
  "One view shown twice retains each window's distinct composer offset."
  (mevedel-view-test--with-buffers
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer view-buf)
      (mevedel-view-test--insert-composer-draft "shared view draft" 2)
      (let ((first-window (selected-window))
            (second-window (split-window-right)))
        (set-window-point second-window (+ (mevedel-view--input-start) 7))
        (with-current-buffer data-buf
          (mevedel-view-call-preserving-window-configuration
           (lambda ()
             (with-current-buffer view-buf (mevedel-view--full-rerender))
             (select-window second-window)
             (delete-other-windows)
             (switch-to-buffer data-buf))))
        (should (eq (selected-window) first-window))
        (should (eq (window-buffer first-window) view-buf))
        (should (eq (window-buffer second-window) view-buf))
        (should (= 2 (- (window-point first-window) (mevedel-view--input-start))))
        (should (= 7 (- (window-point second-window) (mevedel-view--input-start))))))))

(provide 'test-mevedel-view-background-cursor)
;;; test-mevedel-view-background-cursor.el ends here
