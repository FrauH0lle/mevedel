;;; test-mevedel-overlays-coextensive.el --- Equal range geometry tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Replacement may give unrelated presentations the same geometric range.
;; Equal ranges must not form a cyclic ancestry or lose note identities.

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-notes)
(require 'mevedel-overlay-ui)
(require 'mevedel-directive-source)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-instruction-test-support"))

(mevedel-deftest mevedel--parent-instruction
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global))
   :quiet t)
  ,test
  (test)
  :doc "coextensive overlays share a strict outer parent without parenting each other"
  (with-temp-buffer
    (insert "abcdefghij")
    (let ((outer (mevedel-instruction-test--create-note-in (current-buffer) 1 11 'char))
          (first (mevedel-instruction-test--create-note-in (current-buffer) 3 7 'char))
          (second (mevedel-instruction-test--create-note-in (current-buffer) 3 7 'char)))
      (dolist (overlay (list first second))
        (should (eq outer (mevedel--parent-instruction overlay)))
        (should (eq outer (mevedel--topmost-instruction overlay))))
      (mevedel--update-instruction-overlay outer t)
      (should (= 3 (length (mevedel--instructions-in 1 11))))))
  :doc "combined parent and nested note replacement cannot cycle or lose identities"
  (with-temp-buffer
    (insert "before target after\n")
    (let* ((buffer (current-buffer))
           (parent (mevedel-instruction-test--create-note-in buffer 8 14 'char))
           (child (mevedel-instruction-test--create-note-in buffer 9 13 'char))
           (parent-id (overlay-get parent 'mevedel-id))
           (child-id (overlay-get child 'mevedel-id)))
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
        (should-not (mevedel--parent-instruction overlay))
        (should (eq overlay (mevedel--topmost-instruction overlay))))
      (undo-boundary)
      (undo-only 1)
      (should (= 8 (overlay-start parent)))
      (should (= 14 (overlay-end parent)))
      (should (= 9 (overlay-start child)))
      (should (= 13 (overlay-end child)))
      (should (= parent-id (overlay-get parent 'mevedel-id)))
      (should (= child-id (overlay-get child 'mevedel-id)))
      (should (eq parent (mevedel--parent-instruction child))))))

(provide 'test-mevedel-overlays-coextensive)
;;; test-mevedel-overlays-coextensive.el ends here
