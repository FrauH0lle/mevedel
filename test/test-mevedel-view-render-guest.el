;;; test-mevedel-view-render-guest.el --- Guest turn heading tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for the collaboration guest heading lookup in the
;; rendered view.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-transcript-audit)
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
        (search-forward "Shared context")
        (should (get-text-property (point) 'mevedel-view-collapsed))
        (should-not (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (mevedel-view-toggle-section)
        (should (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (equal "> Keep this draft\nsecond line" (mevedel-view--input-text))))
      (mevedel-view-test--insert-data data-buf "It describes your information.\n" 'response)
      (with-current-buffer view-buf
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Shared context")
        (should-not (get-text-property (point) 'mevedel-view-collapsed))
        (should (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (mevedel-view-toggle-section)
        (mevedel-view--full-rerender)
        (goto-char (point-min))
        (search-forward "Shared context")
        (should (get-text-property (point) 'mevedel-view-collapsed))
        (should-not (string-match-p "board.png" (buffer-substring-no-properties (point-min) mevedel-view--input-marker)))
        (should (equal "> Keep this draft\nsecond line" (mevedel-view--input-text))))
      (with-current-buffer data-buf
        (should (string-prefix-p before (buffer-substring-no-properties (point-min) (point-max))))))))

;;; test-mevedel-view-render-guest.el ends here
