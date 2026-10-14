;;; test-mevedel-view-render-insets.el --- Disclosure wrap insets -*- lexical-binding: t -*-

;;; Commentary:

;; The view owns both first-line and continuation indentation, even when
;; text-mode hooks enable automatic wrapping modes.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tool-ui)
(require 'mevedel-view)
(require 'mevedel-view-disclosure)
(require 'mevedel-view-render)
(require 'visual-wrap)

(mevedel-deftest mevedel-view-mode ()
  ,test
  (test)
  :doc "mode hooks cannot leave automatic wrapping in charge of view prefixes"
  (let ((text-mode-hook (list (lambda () (visual-wrap-prefix-mode 1)))))
    (mevedel-view-test--with-buffers
      (with-current-buffer view-buf
        (should visual-line-mode)
        (should-not visual-wrap-prefix-mode)))))

(mevedel-deftest mevedel-view-render-insets ()
  ,test
  (test)
  :doc "SendMessage and other UI tools align body text with their parent label"
  (dolist (name '("SendMessage" "FollowupAgent" "ToolSearch"))
    (dolist (indent '("" "  " "    " "┌ "))
      (mevedel-view-test--with-buffers
        (mevedel-tool-ui--register)
        (with-current-buffer view-buf
          (let* ((body "First payload line with enough text to wrap.\n  Authored indent.")
                 (source
                  (with-current-buffer data-buf
                    (let ((start (point)))
                      (insert (format "(:name %S :args (:message %S :query \"test\"))\n\n%s\n"
                                      name body body)
                              (mevedel-tool-render-data-format
                               '(:kind collaboration-event :event interacted :path "/root/test")
                               "call/1"))
                      (put-text-property start (point) 'gptel '(tool . "call/1"))
                      (cons start (point)))))
                 (child (list :id "call/1" :tool name :status 'success
                              :args (list :message body :query "test") :result body
                              :render-data '(:kind collaboration-event :event interacted
                                             :path "/root/test")))
                 (inset (concat (make-string (string-width indent) ?\s) "    ")))
            (let ((inhibit-read-only t))
              (goto-char mevedel-view--input-marker)
              (if (string-empty-p indent)
                  (mevedel-view--insert-rendered-tool
                   (plist-put (mevedel-view--child-call-rendering child)
                              :vtype 'tool-summary) source)
                (mevedel-view--insert-child-call-block child source t indent)))
            (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 3)
            (save-excursion
              (goto-char (point-min))
              (search-forward (concat name ":"))
              (should (equal (unless (string-empty-p indent) indent)
                             (get-text-property (match-beginning 0) 'line-prefix)))
              (mevedel-view-toggle-section)
              (dotimes (_ 2)
                (goto-char (point-min))
                (search-forward "First payload")
                (let ((start (match-beginning 0)))
                  (should (equal inset (get-text-property start 'line-prefix)))
                  (should (equal inset (get-text-property start 'wrap-prefix))))
                (search-forward "  Authored indent.")
                (should (equal inset (get-text-property (match-beginning 0) 'wrap-prefix)))
                (goto-char (point-min))
                (search-forward (concat name ":"))
                (should (equal (unless (string-empty-p indent) indent)
                               (get-text-property (match-beginning 0) 'line-prefix)))
                (mevedel-view-toggle-section)
                (mevedel-view-toggle-section)))
            (should (equal "> keep\nsecond line" (mevedel-view--input-text)))))))))

(mevedel-deftest mevedel-view-render-mailbox-insets ()
  ,test
  (test)
  :doc "delivered message continuations align after the gutter without copying it"
  (dolist (tag '("agent-message" "agent-result"))
    (dolist (indent '("" "  " "┌ "))
      (mevedel-view-test--with-buffers
        (with-current-buffer data-buf (insert "source\n"))
        (with-current-buffer view-buf
          (let* ((source (cons 1 8))
                 (body "First payload line with long prose to wrap.\n  Authored indent.")
                 (text (format "<%s sender=\"/root/test\" outcome=\"completed\">\n%s\n</%s>\n"
                               tag body tag))
                 (child (list :kind 'mailbox :source source
                              :rendering (list :mailbox-text text)))
                 (prefix (concat (make-string (string-width indent) ?\s) "    │ "))
                 (wrap (make-string (+ (string-width indent) 6) ?\s)))
            (let ((inhibit-read-only t))
              (goto-char mevedel-view--input-marker)
              (if (string-empty-p indent)
                  (progn
                    (insert text)
                    (mevedel-view--decorate-agent-message-blocks (point-min) (point))
                    (mevedel-view--decorate-agent-result-blocks (point-min) (point)))
                (mevedel-view--insert-child-call-block child source nil indent)))
            (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 3)
            (dotimes (_ 3)
              (save-excursion
                (goto-char (point-min))
                (search-forward "/root/test")
                (should (equal (unless (string-empty-p indent) indent)
                               (get-text-property (match-beginning 0) 'line-prefix)))
                (search-forward "First payload")
                (let ((start (match-beginning 0)))
                  (should (equal prefix (get-text-property start 'line-prefix)))
                  (should (equal wrap (get-text-property start 'wrap-prefix)))
                  (beginning-of-line)
                  (should (looking-at-p "First payload")))
                (search-forward "  Authored indent.")
                (should (equal wrap (get-text-property (match-beginning 0) 'wrap-prefix)))
                (mevedel-view-toggle-section)
                (mevedel-view-toggle-section))
              (should (equal "> keep\nsecond line" (mevedel-view--input-text)))))))))

  :doc "collapsed hints keep the nested header inset across decoration and toggles"
  (dolist (tag '("agent-message" "agent-result"))
    (dolist (indent '("" "  " "┌ "))
      (mevedel-view-test--with-buffers
        (with-current-buffer data-buf (insert "source\n"))
        (with-current-buffer view-buf
          (let* ((source (cons 1 8))
                 (text (format "<%s sender=\"/root/test\" outcome=\"completed\">\nPayload line.\n</%s>\n"
                               tag tag))
                 (child (list :kind 'mailbox :source source
                              :rendering (list :mailbox-text text)))
                 (wrap (unless (string-empty-p indent)
                         (make-string (string-width indent) ?\s))))
            (let ((inhibit-read-only t))
              (goto-char mevedel-view--input-marker)
              (if (string-empty-p indent)
                  (let ((mevedel-view-mailbox-collapse-line-threshold 0))
                    (insert text)
                    (mevedel-view--decorate-agent-message-blocks (point-min) (point))
                    (mevedel-view--decorate-agent-result-blocks (point-min) (point)))
                (mevedel-view--insert-child-call-block child source t indent)))
            (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 3)
            (let ((draft-point (point)))
              (cl-flet ((check-hint ()
                          (goto-char (point-min))
                          (should (search-forward "[1 line collapsed]"
                                                  mevedel-view--input-marker t))
                          (let ((hint (match-beginning 0)))
                            (should (equal (unless (string-empty-p indent) indent)
                                           (get-text-property hint 'line-prefix)))
                            (should (equal wrap (get-text-property hint 'wrap-prefix)))
                            (should-not (invisible-p hint)))))
                ;; Initial decoration, then interactive re-collapse.
                (dotimes (_ 3)
                  (save-excursion
                    (check-hint)
                    (mevedel-view-toggle-section)
                    (goto-char (point-min))
                    (should-not (search-forward "collapsed]"
                                                mevedel-view--input-marker t))
                    (search-forward "/root/test")
                    (mevedel-view-toggle-section)
                    (check-hint))))
              (should (equal "> keep\nsecond line" (mevedel-view--input-text)))
              (should (= draft-point (point))))))))))

(mevedel-deftest mevedel-view-render-mailbox-boundaries ()
  ,test
  (test)
  :doc "collapsed cards retain a visible terminator before any following tool"
  (dolist (tag '("agent-message" "agent-result"))
    (dolist (name '("Read" "ToolSearch" "SendMessage" "Grep" "Bash"))
      (dolist (following-indent '("" "  " "┌ "))
        (mevedel-view-test--with-buffers
          (mevedel-tool-ui--register)
          (with-current-buffer data-buf (insert "source\nsource\n"))
          (with-current-buffer view-buf
            (let* ((body "  Authored first line.\nSecond line.\n\n")
                   ;; Activity entries trim the closing tag's trailing newline.
                   ;; An extra newline here would mask the display boundary bug.
                   (text (format "<%s sender=\"/root/test\" outcome=\"completed\">\n%s</%s>"
                                 tag body tag))
                   (child (list :kind 'mailbox
                                :rendering (list :mailbox-text text)))
                   (following
                    (list :id "following" :tool name :status 'success
                          :args '(:file_path "after.el" :query "test"
                                  :message "test" :pattern "test" :command "true")
                          :result "content"
                          :render-data
                          (when (equal name "SendMessage")
                            '(:kind collaboration-event :event interacted
                              :path "/root/following"))))
                   (inhibit-read-only t))
              (goto-char mevedel-view--input-marker)
              (mevedel-view--insert-child-call-block child '(1 . 8) t "  ")
              (mevedel-view--insert-child-call-block
               following '(8 . 15) t following-indent)
              (mevedel-view-test--insert-composer-draft "> keep\nsecond line" 3)
              (dotimes (_ 3)
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "/root/test")
                  (let* ((bounds (mevedel-view-disclosure--mailbox-bounds))
                         (header-end (line-end-position))
                         (end (cdr bounds))
                         (payload (mevedel-view-disclosure--mailbox-body-text
                                   (car bounds) end)))
                    (should (equal (concat "\n" body) payload))
                    ;; One visible newline: the payload terminator, not the
                    ;; header's separator followed by entirely hidden lines.
                    (should (invisible-p header-end))
                    (should (eq ?\n (char-before end)))
                    (should-not (invisible-p (1- end)))
                    (should-not (get-text-property header-end 'mevedel-view-mailbox-body))
                    (should (get-text-property (1- end) 'mevedel-view-mailbox-body))
                    (mevedel-view-toggle-section)
                    (should-not (invisible-p (line-end-position)))
                    (mevedel-view-toggle-section))
                  (goto-char (point-min))
                  (search-forward (concat name ":"))
                  (let ((start (line-beginning-position)))
                    (should (equal (unless (string-empty-p following-indent)
                                     following-indent)
                                   (get-text-property start 'line-prefix)))
                    (should-not (invisible-p (1- start)))
                    (should-not (invisible-p (line-end-position)))))
                (should (equal "> keep\nsecond line" (mevedel-view--input-text)))))))))))

(provide 'test-mevedel-view-render-insets)
;;; test-mevedel-view-render-insets.el ends here
