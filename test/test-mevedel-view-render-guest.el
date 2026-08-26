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

(mevedel-deftest mevedel-view--user-turn-guest-name
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
        (should (equal "Herr Boing"
                       (mevedel-view--user-turn-guest-name
                        (list (list 'user 1 prompt-end))
                        (current-buffer))))
        ;; Absorbed: segment repair grew the span over the audit strip.
        (should (equal "Herr Boing"
                       (mevedel-view--user-turn-guest-name
                        (list (list 'user 1 strip-end))
                        (current-buffer))))
        ;; A host turn after the strip is not attributed.
        (should-not (mevedel-view--user-turn-guest-name
                     (list (list 'user strip-end (point-max)))
                     (current-buffer)))
        ;; No segments, no lookup.
        (should-not (mevedel-view--user-turn-guest-name
                     nil (current-buffer)))))))

;;; test-mevedel-view-render-guest.el ends here
