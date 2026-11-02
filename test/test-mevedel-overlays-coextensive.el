;;; test-mevedel-overlays-coextensive.el --- Equal range geometry tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Replacement may give unrelated presentations the same geometric range.
;; Equal ranges must not form a cyclic ancestry or delete authored records.

;;; Code:

(require 'mevedel-overlays)
(require 'mevedel-overlay-ui)
(require 'mevedel-directive-source)
(require 'mevedel-directive)
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
    (let ((outer (mevedel--create-reference-in (current-buffer) 1 11 'char))
          (first (mevedel--create-reference-in (current-buffer) 3 7 'char))
          (second (mevedel--create-reference-in (current-buffer) 3 7 'char)))
      (dolist (overlay (list first second))
        (should (eq outer (mevedel--parent-instruction overlay)))
        (should (eq outer (mevedel--topmost-instruction overlay))))
      (mevedel--update-instruction-overlay outer t)
      (should (= 3 (length (mevedel--instructions-in 1 11))))))
  :doc "combined parent and nested directive replacement cannot cycle or delete records"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (buffer (nth 3 fixture))
         (parent (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((child (mevedel--create-directive-in buffer 9 13 nil "Nested" 'char))
                 (nested (mevedel--subdirective-record child))
                 (parent-id (overlay-get parent 'mevedel-id))
                 (child-id (overlay-get child 'mevedel-id)))
            (setf (mevedel-directive-attempts record)
                  (list (mevedel-instruction-test--attempt)))
            (mevedel-directive-set-state record 'implemented)
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
            (should (eq parent (mevedel--topmost-instruction child 'directive)))
            (should (eq 'implemented (mevedel--directive-state child)))
            (save-restriction
              (narrow-to-region 1 7)
              (should (eq parent (mevedel--topmost-instruction child 'directive)))
              (should (eq 'implemented (mevedel--directive-state child))))
            (goto-char 9)
            (let ((mevedel--highlighted-instruction child))
              (should (eq parent (mevedel--directive-at-point))))
            (should (eq record (mevedel--directive-record parent)))
            (should (memq nested (mevedel-directive-subdirectives record)))
            (should (= 1 (length (mevedel-directive-attempts record))))
            (undo-boundary)
            (undo-only 1)
            (should (= 8 (overlay-start parent)))
            (should (= 14 (overlay-end parent)))
            (should (= 9 (overlay-start child)))
            (should (= 13 (overlay-end child)))
            (should (= parent-id (overlay-get parent 'mevedel-id)))
            (should (= child-id (overlay-get child 'mevedel-id)))
            (should (eq parent (mevedel--parent-instruction child)))))
      (mevedel-instruction-test--discard-source fixture)))
  :doc "source-missing recovery restores coextensive nested identities and ownership"
  (let* ((fixture (mevedel-instruction-test--source-fixture))
         (workspace (nth 1 fixture))
         (buffer (nth 3 fixture))
         (parent (nth 4 fixture))
         (record (nth 5 fixture)))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((child (mevedel--create-directive-in buffer 9 13 nil "Nested" 'char))
                 (nested (mevedel--subdirective-record child))
                 (parent-id (overlay-get parent 'mevedel-id))
                 (child-id (overlay-get child 'mevedel-id)))
            (combine-change-calls 8 14
              (delete-region 8 14)
              (goto-char 8)
              (insert "new"))
            (mevedel--mark-buffer-source-missing buffer)
            (should-not (overlay-buffer parent))
            (should-not (overlay-buffer child))
            (should (= 2 (mevedel--restore-source-missing-directives buffer)))
            (let ((restored-parent (mevedel--instruction-with-uuid
                                    (mevedel-directive-id record) workspace))
                  (restored-child (mevedel--instruction-with-uuid
                                   (mevedel-subdirective-id nested) workspace)))
              (dolist (overlay (list restored-parent restored-child))
                (should (eq buffer (overlay-buffer overlay)))
                (should (= 8 (overlay-start overlay)))
                (should (= 11 (overlay-end overlay))))
              (should (= parent-id (overlay-get restored-parent 'mevedel-id)))
              (should (= child-id (overlay-get restored-child 'mevedel-id)))
              (should (eq restored-parent
                          (mevedel--topmost-instruction restored-child 'directive))))))
      (mevedel-instruction-test--discard-source fixture))))

(provide 'test-mevedel-overlays-coextensive)
;;; test-mevedel-overlays-coextensive.el ends here
