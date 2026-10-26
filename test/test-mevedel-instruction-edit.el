;;; test-mevedel-instruction-edit.el --- Line mapped rewrite tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Check the shared synchronization surface used by patches and restoration.

;;; Code:

(require 'mevedel-instruction-edit)
(require 'mevedel-overlays)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-instruction-replace-buffer-contents
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)
          (last-command nil))
   :quiet t)
  ,test
  (test)
  :doc "maps changed lines without swallowing adjacent context or insertions"
  (dolist (case '(("above\nbefore\n# selected NEW\nafter\nbelow\n" "# selected NEW\n")
                  ("BEFORE\n# selected NEW\nAFTER\n" "# selected NEW\n")
                  ("before\nreplacement\nextra\nafter\n" "replacement\nextra\n")
                  ("before\nSEL\nnew\nAFTER\n" "SEL\nnew\n")
                  ("BEFORE\nnew\nSELECTED\nafter\n" "new\nSELECTED\n")
                  ("one replacement\n" "one replacement\n")
                  ("before\nafter\n" nil)
                  ("before\nselected" "selected")
                  ("before\nselected\n" "selected\n")))
    (let ((target (generate-new-buffer " *rewrite target*"))
          (source (generate-new-buffer " *rewrite source*")))
      (unwind-protect
          (with-current-buffer target
            (insert "before\nselected\nafter\n")
            (let ((overlay (mevedel--create-reference-in target 8 17)))
              (with-current-buffer source (insert (car case)))
              (mevedel-instruction-replace-buffer-contents source)
              (should (equal (car case) (buffer-string)))
              (if (cadr case)
                  (should (equal (cadr case)
                                 (buffer-substring-no-properties
                                  (overlay-start overlay) (overlay-end overlay))))
                (should-not (overlay-buffer overlay)))))
        (kill-buffer target)
        (kill-buffer source))))
  :doc "line mapped deletion supports native undo and redo"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let* ((target (current-buffer))
           (overlay (mevedel--create-reference-in target 8 17)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (with-temp-buffer
        (insert "before\nafter\n")
        (let ((source (current-buffer)))
          (with-current-buffer target
            (mevedel-instruction-replace-buffer-contents source))))
      (should-not (overlay-buffer overlay))
      (undo-boundary)
      (undo-only 1)
      (should (= 8 (overlay-start overlay)))
      (should (= 17 (overlay-end overlay)))
      (undo-boundary)
      (undo-redo 1)
      (should-not (overlay-buffer overlay))))
  :doc "undo and redo retain the unequal-run line mapping"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let* ((target (current-buffer))
           (overlay (mevedel--create-reference-in target 8 17)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (with-temp-buffer
        (insert "before\nSEL\nnew\nAFTER\n")
        (let ((source (current-buffer)))
          (with-current-buffer target
            (mevedel-instruction-replace-buffer-contents source))))
      (undo-boundary)
      (undo-only 1)
      (should (equal "before\nselected\nafter\n" (buffer-string)))
      (should (= 8 (overlay-start overlay)))
      (should (= 17 (overlay-end overlay)))
      (undo-boundary)
      (undo-redo 1)
      (should (equal "SEL\nnew\n"
                     (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))))))
  :doc "preserves exact contents without instructions, including an empty source"
  (with-temp-buffer
    (insert "old\n")
    (let ((target (current-buffer)))
      (with-temp-buffer
        (let ((source (current-buffer)))
          (with-current-buffer target
            (mevedel-instruction-replace-buffer-contents source)
            (should (equal "" (buffer-string)))))))))

(provide 'test-mevedel-instruction-edit)
;;; test-mevedel-instruction-edit.el ends here
