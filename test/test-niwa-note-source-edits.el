;;; test-niwa-note-source-edits.el --- Instruction edit geometry -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise real change hooks, persisted shape, and undo.

;;; Code:

(require 'niwa-notes)

(require 'niwa-note-source)
(require 'niwa-overlays)
(require 'niwa-persistence)
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

(niwa-deftest niwa--instruction-after-change
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "line spans retain whole lines under boundary and intersecting edits"
  (dolist (case '((8 8 "prefix " "prefix selected\n")
                  (8 8 "above\n" "selected\n")
                  (17 17 "below\n" "selected\n")
                  (11 11 "NEW\n" "selNEW\nected\n")
                  (10 13 "" "seted\n")
                  (5 10 "" "befolected\n")
                  (14 20 "" "selecter")
                  (7 8 "" "beforeselected\n")
                  (16 17 "" "selectedafter")
                  (8 16 "replacement" "replacement\n")))
    (pcase-let ((`(,beg ,end ,text ,expected) case))
      (with-temp-buffer
        (insert "before\nselected\nafter\n")
        (let ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 17)))
          (combine-change-calls beg end
            (delete-region beg end)
            (goto-char beg)
            (insert text))
          (should (equal expected
                         (buffer-substring-no-properties
                          (overlay-start overlay) (overlay-end overlay))))) )))
  :doc "character spans grow across intersecting replacements, not adjacent insertions"
  (dolist (case '((4 7 "replacement" 3 15)
                  (3 3 "adjacent" 11 14)
                  (6 6 "adjacent" 3 6)))
    (pcase-let ((`(,beg ,end ,text ,start ,finish) case))
      (with-temp-buffer
        (insert "0123456789")
        (let ((overlay (niwa-instruction-test--create-note-in (current-buffer) 3 6)))
          (combine-change-calls beg end
            (delete-region beg end)
            (goto-char beg)
            (insert text))
          (should (= start (overlay-start overlay)))
          (should (= finish (overlay-end overlay))))))))

(niwa-deftest niwa--create-instruction-overlay-in
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "infers and persists shape, allowing explicit linewise selections"
  (with-temp-buffer
    (insert "first\nsecond\nthird\n")
    (dolist (case '((7 13 nil line) (7 14 nil line)
                    (8 12 nil char) (8 12 line line) (7 14 char char)))
      (pcase-let* ((`(,beg ,end ,shape ,expected) case)
                  (overlay (niwa--create-instruction-overlay-in
                            (current-buffer) beg end shape)))
        (should (eq expected (overlay-get overlay 'niwa-span)))
        (should (eq expected
                    (plist-get (niwa--instruction-persisted-properties overlay)
                               'niwa-span)))))))

(niwa-deftest niwa--instruction-initialize-overlay
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)))
  ,test
  (test)
  :doc "restored instructions keep explicit shape and infer missing saved shape"
  (dolist (span '(line char nil))
    (with-temp-buffer
      (insert "before\nselected\nafter\n")
      (let* ((original (niwa-instruction-test--create-note-in (current-buffer) 8 17))
             (properties (niwa--instruction-persisted-properties original)))
        (setq properties (plist-put properties 'niwa-span span))
        (delete-overlay original)
        (let ((restored (niwa--restore-overlay (current-buffer) 8 17 properties)))
          (should (eq (or span 'line) (overlay-get restored 'niwa-span)))
          (goto-char 8)
          (insert "# ")
          (should (= (if (eq span 'char) 10 8) (overlay-start restored))))))))

(provide 'test-niwa-note-source-edits)
;;; test-niwa-note-source-edits.el ends here
