;;; test-niwa-overlays-coextensive.el --- Equal range geometry tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Replacement may give unrelated presentations the same geometric range.
;; Equal ranges must not form a cyclic ancestry or lose note identities.

;;; Code:

(require 'niwa-overlays)
(require 'niwa-notes)
(require 'niwa-overlay-ui)
(require 'niwa-note-source)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(niwa-deftest niwa--parent-instruction
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global))
   :quiet t)
  ,test
  (test)
  :doc "coextensive overlays share a strict outer parent without parenting each other"
  (with-temp-buffer
    (insert "abcdefghij")
    (let ((outer (niwa-instruction-test--create-note-in (current-buffer) 1 11 'char))
          (first (niwa-instruction-test--create-note-in (current-buffer) 3 7 'char))
          (second (niwa-instruction-test--create-note-in (current-buffer) 3 7 'char)))
      (dolist (overlay (list first second))
        (should (eq outer (niwa--parent-instruction overlay)))
        (should (eq outer (niwa--topmost-instruction overlay))))
      (niwa--update-instruction-overlay outer t)
      (should (= 3 (length (niwa--instructions-in 1 11))))))
  :doc "combined parent and nested note replacement cannot cycle or lose identities"
  (with-temp-buffer
    (insert "before target after\n")
    (let* ((buffer (current-buffer))
           (parent (niwa-instruction-test--create-note-in buffer 8 14 'char))
           (child (niwa-instruction-test--create-note-in buffer 9 13 'char))
           (parent-id (overlay-get parent 'niwa-id))
           (child-id (overlay-get child 'niwa-id)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (combine-change-calls 8 14
        (delete-region 8 14)
        (goto-char 8)
        (insert "new"))
      (dolist (overlay (list parent child))
        (should (eq buffer (overlay-buffer overlay)))
        (should (= 8 (overlay-start overlay)))
        (should (= 11 (overlay-end overlay)))
        (should-not (niwa--parent-instruction overlay))
        (should (eq overlay (niwa--topmost-instruction overlay))))
      (undo-boundary)
      (undo-only 1)
      (should (= 8 (overlay-start parent)))
      (should (= 14 (overlay-end parent)))
      (should (= 9 (overlay-start child)))
      (should (= 13 (overlay-end child)))
      (should (= parent-id (overlay-get parent 'niwa-id)))
      (should (= child-id (overlay-get child 'niwa-id)))
      (should (eq parent (niwa--parent-instruction child))))))

(provide 'test-niwa-overlays-coextensive)
;;; test-niwa-overlays-coextensive.el ends here
