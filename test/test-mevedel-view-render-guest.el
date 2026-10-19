;;; test-mevedel-view-render-guest.el --- Guest turn heading tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for the collaboration guest heading lookup in the
;; rendered view, and the background-event turn, which renders no heading or
;; trigger text, that shares it.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-transcript-audit)
(require 'mevedel-view)
(require 'mevedel-view-render)

(mevedel-deftest mevedel-view--user-turn-attribution
  (:doc "finds the guest in the turn's trailing audit strip and inside an absorbed span")
  (with-temp-buffer
    (insert "How many moons has Saturn?\n")
    (let ((prompt-end (point)))
      ;; Another audit type first, then the attribution, as the composer
      ;; writes them.
      (insert (mevedel--format-hook-audit-record
               (list :type 'prompt-rewrite :event "UserPromptSubmit"
                     :original "a" :submitted "b")))
      (insert (mevedel--format-hook-audit-record
               (list :type 'guest-prompt :name "Herr Boing")))
      (let ((strip-end (point)))
        (insert "The response text.\n")
        ;; Trailing strip: the segment ends at the prompt.
        (should (equal '(:type guest-prompt :name "Herr Boing")
                       (mevedel-view--user-turn-attribution
                        (list (list 'user 1 prompt-end))
                        (current-buffer))))
        ;; Absorbed: segment repair grew the span over the audit strip.
        (should (equal '(:type guest-prompt :name "Herr Boing")
                       (mevedel-view--user-turn-attribution
                        (list (list 'user 1 strip-end))
                        (current-buffer))))
        ;; A host turn after the strip is not attributed.
        (should-not (mevedel-view--user-turn-attribution
                     (list (list 'user strip-end (point-max)))
                     (current-buffer)))
        ;; No segments, no lookup.
        (should-not (mevedel-view--user-turn-attribution
                     nil (current-buffer)))))))

(mevedel-deftest mevedel-view--insert-user-message
  (:doc "a background-event turn renders no heading or trigger live and after a full rerender")
  (mevedel-view-test--with-buffers
    (let ((trigger "Background work you started has reported.")
          (attribution (list :type 'background-event)))
      (with-current-buffer data-buf
        (insert trigger "\n")
        (insert (mevedel--format-hook-audit-record attribution)))
      (mevedel-view-test--insert-data data-buf "Handled.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft "> Keep this draft\nsecond line" 4)
        (let ((offset (- (point) (mevedel-view--input-start))))
          (mevedel-view--insert-user-message trigger nil nil nil nil nil attribution)
          (let ((history (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "Background event" history))
            (should-not (string-match-p (regexp-quote trigger) history))
            (should-not (string-match-p "^You$" history)))
          (mevedel-view--full-rerender)
          (let ((history (buffer-substring-no-properties
                          (point-min) mevedel-view--input-marker)))
            (should-not (string-match-p "Background event" history))
            (should-not (string-match-p (regexp-quote trigger) history))
            (should (string-match-p "Handled\\." history))
            (should-not (string-match-p "^You$" history))
            (should-not (string-match-p "(guest)" history)))
          (should (equal "> Keep this draft\nsecond line" (mevedel-view--input-text)))
          (should (= offset (- (point) (mevedel-view--input-start)))))))))


(mevedel-deftest mevedel-view--render-user-turn/empty-background
  (:doc "a hidden background wake contributes no text or navigable source span")
  (mevedel-view-test--with-buffers
    (let (before)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (setq before (buffer-substring-no-properties
                      (point-min) (mevedel-view--history-insertion-marker))))
      (with-current-buffer data-buf
        (insert "Background work you started has reported.\n")
        (insert (mevedel--format-hook-audit-record '(:type background-event))))
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (should (equal before (buffer-substring-no-properties
                               (point-min) (mevedel-view--history-insertion-marker))))
        (should-not (text-property-not-all
                     (point-min) (mevedel-view--history-insertion-marker)
                     'mevedel-view-turn-id nil))))))

(mevedel-deftest mevedel-view--render-user-turn/background-live
  (:doc "a hidden wake stays absent through retained live updates and reconstruction")
  (mevedel-view-test--with-buffers
    (let (start live)
      (with-current-buffer view-buf
        (setq start (marker-position (mevedel-view--history-insertion-marker)))
        (mevedel-view-test--insert-composer-draft "> Keep this draft\nsecond line" 4)
        (mevedel-view-stream-begin-turn
         (copy-marker start)
         (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (insert "Background work you started has reported.\n")
        (insert (mevedel--format-hook-audit-record '(:type background-event))))
      (mevedel-view-test--insert-data data-buf "First paragraph.\n\nMutable tail" 'response)
      (with-current-buffer view-buf
        (mevedel-view-render-live-update data-buf))
      (mevedel-view-test--insert-data data-buf " continues.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view-render-live-update data-buf)
        (should (equal "Assistant" (buffer-substring-no-properties start (+ start 9))))
        (should (equal "> Keep this draft\nsecond line" (mevedel-view--input-text)))
        (should (= 4 (- (point) (mevedel-view--input-start))))
        (setq live (buffer-substring-no-properties
                    (point-min) (mevedel-view--history-insertion-marker)))
        (mevedel-view--full-rerender)
        (should (equal live (buffer-substring-no-properties
                             (point-min) (mevedel-view--history-insertion-marker))))))))

(mevedel-deftest mevedel-view--render-user-turn/background-reminder
  (:doc "background wakes retain visible reminder rows without a generated heading")
  (mevedel-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "Background work you started has reported.\n")
      (insert (mevedel--format-hook-audit-record '(:type background-event)))
      (insert (mevedel--format-hook-audit-record
               '(:type injected-reminders :phase turn-start
                 :items ((:type verification-suggestion :body "Check the result."))))))
    (mevedel-view-test--insert-data data-buf "Handled.\n" 'response)
    (with-current-buffer view-buf
      (mevedel-view--full-rerender)
      (let ((history (buffer-substring-no-properties
                      (point-min) (mevedel-view--history-insertion-marker))))
        (should (string-match-p "1 system reminder" history))
        (should (string-match-p "Handled\\." history))
        (should-not (string-match-p "Background work\\|^You$" history)))
      (goto-char (point-min))
      (search-forward "1 system reminder")
      (should (get-text-property (1- (point)) 'mevedel-view-source))
      (should (get-text-property (1- (point)) 'mevedel-view-turn-id))
      (mevedel-view-toggle-section)
      (should (string-match-p "Check the result\\." (buffer-string))))))

(mevedel-deftest mevedel-view--insert-shared-context
  (:doc "Shared context folds in live echoes and full renders, retaining expansion and the composer")
  (mevedel-view-test--with-buffers
    (let* ((question "What is a data model?")
           (context "Shared content snapshot (user-provided data):\n{\"content\":\"data model\"}\n[[file:/tmp/board.png]]")
           (text (concat question "\n\n" context))
           (attribution (list :type 'guest-prompt :name "Joey"
                              :shared (list :text question :title "Notes" :scope "selection" :revision 5)))
           source before)
      (with-current-buffer data-buf
        (insert text "\n")
        (setq source (mevedel-view-disclosure-source-range data-buf 1 (point)))
        (insert (mevedel--format-hook-audit-record attribution))
        (setq before (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft "> Keep this draft\nsecond line" 4)
        (mevedel-view--insert-user-message text nil nil nil nil nil attribution source)
        (goto-char (point-min))
        (search-forward question)
        (search-forward "Discussion · ")
        (should (get-text-property (point) 'mevedel-view-collapsed))
        (should-not (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (mevedel-view-toggle-section)
        (should (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (equal "> Keep this draft\nsecond line" (mevedel-view--input-text))))
      (mevedel-view-test--insert-data data-buf "It describes your information.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Discussion · ")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (should (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (mevedel-view-toggle-section)
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Discussion · ")
        (should (get-text-property (point) 'mevedel-view-collapsed))
        (should-not (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (equal "> Keep this draft\nsecond line" (mevedel-view--input-text))))
      (with-current-buffer data-buf
        (should (string-prefix-p before (buffer-substring-no-properties (point-min) (point-max))))))))

;;; test-mevedel-view-render-guest.el ends here
