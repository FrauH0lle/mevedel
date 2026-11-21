;;; test-niwa-view-render-guest.el --- Guest turn heading tests -*- lexical-binding: t; -*-

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
(require 'niwa-transcript-audit)
(require 'niwa-view)
(require 'niwa-view-render)

(niwa-deftest niwa-view--user-turn-attribution
  (:doc "finds the guest in the turn's trailing audit strip and inside an absorbed span")
  (with-temp-buffer
    (insert "How many moons has Saturn?\n")
    (let ((prompt-end (point)))
      ;; Another audit type first, then the attribution, as the composer
      ;; writes them.
      (insert (niwa--format-hook-audit-record
               (list :type 'prompt-rewrite :event "UserPromptSubmit"
                     :original "a" :submitted "b")))
      (insert (niwa--format-hook-audit-record
               (list :type 'guest-prompt :name "Herr Boing")))
      (let ((strip-end (point)))
        (insert "The response text.\n")
        ;; Trailing strip: the segment ends at the prompt.
        (should (equal '(:type guest-prompt :name "Herr Boing")
                       (niwa-view--user-turn-attribution
                        (list (list 'user 1 prompt-end))
                        (current-buffer))))
        ;; Absorbed: segment repair grew the span over the audit strip.
        (should (equal '(:type guest-prompt :name "Herr Boing")
                       (niwa-view--user-turn-attribution
                        (list (list 'user 1 strip-end))
                        (current-buffer))))
        ;; A host turn after the strip is not attributed.
        (should-not (niwa-view--user-turn-attribution
                     (list (list 'user strip-end (point-max)))
                     (current-buffer)))
        ;; No segments, no lookup.
        (should-not (niwa-view--user-turn-attribution
                     nil (current-buffer)))))))

(niwa-deftest niwa-view--insert-user-message
  (:doc "a background-event turn renders no heading or trigger live and after a full rerender")
  (niwa-view-test--with-buffers
    (let ((trigger "Background work you started has reported.")
          (attribution (list :type 'background-event)))
      (with-current-buffer data-buf
        (insert trigger "\n")
        (insert (niwa--format-hook-audit-record attribution)))
      (niwa-view-test--insert-data data-buf "Handled.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> Keep this draft\nsecond line" 4)
        (let ((offset (with-current-buffer (niwa-view-composer-buffer)
                        (- (point) (niwa-view-composer-draft-start view-buf)))))
          (niwa-view--insert-user-message trigger nil nil nil nil nil attribution)
          (let ((history (buffer-substring-no-properties
                          (point-min) (niwa-view-transcript-tail))))
            (should-not (string-match-p "Background event" history))
            (should-not (string-match-p (regexp-quote trigger) history))
            (should-not (string-match-p "^You$" history)))
          (niwa-view--full-rerender)
          (let ((history (buffer-substring-no-properties
                          (point-min) (niwa-view-transcript-tail))))
            (should-not (string-match-p "Background event" history))
            (should-not (string-match-p (regexp-quote trigger) history))
            (should (string-match-p "Handled\\." history))
            (should-not (string-match-p "^You$" history))
            (should-not (string-match-p "(guest)" history)))
          (should (equal "> Keep this draft\nsecond line" (niwa-view-composer-draft-text nil t)))
          (should (= offset (with-current-buffer (niwa-view-composer-buffer)
                              (- (point) (niwa-view-composer-draft-start view-buf))))))))))


(niwa-deftest niwa-view--render-user-turn/empty-background
  (:doc "a hidden background wake contributes no text or navigable source span")
  (niwa-view-test--with-buffers
    (let (before)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (setq before (buffer-substring-no-properties
                      (point-min) (niwa-view--history-insertion-marker))))
      (with-current-buffer data-buf
        (insert "Background work you started has reported.\n")
        (insert (niwa--format-hook-audit-record '(:type background-event))))
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (should (equal before (buffer-substring-no-properties
                               (point-min) (niwa-view--history-insertion-marker))))
        (should-not (text-property-not-all
                     (point-min) (niwa-view--history-insertion-marker)
                     'niwa-view-turn-id nil))))))

(niwa-deftest niwa-view--render-user-turn/background-live
  (:doc "a hidden wake stays absent through retained live updates and reconstruction")
  (niwa-view-test--with-buffers
    (let (start live)
      (with-current-buffer view-buf
        (setq start (marker-position (niwa-view--history-insertion-marker)))
        (niwa-view-test--insert-composer-draft "> Keep this draft\nsecond line" 4)
        (niwa-view-stream-begin-turn
         (copy-marker start)
         (with-current-buffer data-buf (copy-marker (point-min)))))
      (with-current-buffer data-buf
        (insert "Background work you started has reported.\n")
        (insert (niwa--format-hook-audit-record '(:type background-event))))
      (niwa-view-test--insert-data data-buf "First paragraph.\n\nMutable tail" 'response)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf))
      (niwa-view-test--insert-data data-buf " continues.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view-render-live-update data-buf)
        (should (equal "Assistant" (buffer-substring-no-properties start (+ start 9))))
        (should (equal "> Keep this draft\nsecond line" (niwa-view-composer-draft-text nil t)))
        (should (= (+ (niwa-view-composer-draft-start) 4)
                   (with-current-buffer (niwa-view-composer-buffer) (point))))
        (setq live (buffer-substring-no-properties
                    (point-min) (niwa-view--history-insertion-marker)))
        (niwa-view--full-rerender)
        (should (equal live (buffer-substring-no-properties
                             (point-min) (niwa-view--history-insertion-marker))))))))

(niwa-deftest niwa-view--render-user-turn/background-reminder
  (:doc "background wakes retain visible reminder rows without a generated heading")
  (niwa-view-test--with-buffers
    (with-current-buffer data-buf
      (insert "Background work you started has reported.\n")
      (insert (niwa--format-hook-audit-record '(:type background-event)))
      (insert (niwa--format-hook-audit-record
               '(:type injected-reminders :phase turn-start
                 :items ((:type verification-suggestion :body "Check the result."))))))
    (niwa-view-test--insert-data data-buf "Handled.\n" 'response)
    (with-current-buffer view-buf
      (niwa-view--full-rerender)
      (let ((history (buffer-substring-no-properties
                      (point-min) (niwa-view--history-insertion-marker))))
        (should (string-match-p "1 system reminder" history))
        (should (string-match-p "Handled\\." history))
        (should-not (string-match-p "Background work\\|^You$" history)))
      (goto-char (point-min))
      (search-forward "1 system reminder")
      (should (get-text-property (1- (point)) 'niwa-view-source))
      (should (get-text-property (1- (point)) 'niwa-view-turn-id))
      (niwa-view-toggle-section)
      (should (string-match-p "Check the result\\." (buffer-string))))))

(niwa-deftest niwa-view--insert-shared-context
  (:doc "Shared context folds in live echoes and full renders, retaining expansion and the composer")
  (niwa-view-test--with-buffers
    (let* ((question "What is a data model?")
           (context "Shared content snapshot (user-provided data):\n{\"content\":\"data model\"}\n[[file:/tmp/board.png]]")
           (text (concat question "\n\n" context))
           (attribution (list :type 'guest-prompt :name "Joey"
                              :shared (list :text question :title "Notes" :scope "selection" :revision 5)))
           source before)
      (with-current-buffer data-buf
        (insert text "\n")
        (setq source (niwa-view-disclosure-source-range data-buf 1 (point)))
        (insert (niwa--format-hook-audit-record attribution))
        (setq before (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft "> Keep this draft\nsecond line" 4)
        (niwa-view--insert-user-message text nil nil nil nil nil attribution source)
        (goto-char (point-min))
        (search-forward question)
        (search-forward "Discussion · ")
        (should (get-text-property (point) 'niwa-view-collapsed))
        (should-not (string-match-p "board.png" (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (niwa-view-toggle-section)
        (should (string-match-p "board.png" (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (should (equal "> Keep this draft\nsecond line" (niwa-view-composer-draft-text nil t))))
      (niwa-view-test--insert-data data-buf "It describes your information.\n" 'response)
      (with-current-buffer view-buf
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Discussion · ")
        (should-not (get-text-property (point) 'niwa-view-collapsed))
        (should (string-match-p "board.png" (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (niwa-view-toggle-section)
        (niwa-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Discussion · ")
        (should (get-text-property (point) 'niwa-view-collapsed))
        (should-not (string-match-p "board.png" (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail))))
        (should (equal "> Keep this draft\nsecond line" (niwa-view-composer-draft-text nil t))))
      (with-current-buffer data-buf
        (should (string-prefix-p before (buffer-substring-no-properties (point-min) (point-max))))))))

;;; test-niwa-view-render-guest.el ends here
