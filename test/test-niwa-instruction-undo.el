;;; test-niwa-instruction-undo.el --- Source deletion undo tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native undo/redo, including registry cleanup and note links.

;;; Code:

(require 'niwa-notes)

(require 'niwa-note-source)
(require 'niwa-overlays)
(require 'niwa-instruction-undo)
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

(niwa-deftest niwa-instruction-undo-capture
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)
          (last-command nil))
   :quiet t)
  ,test
  (test)
  :doc "interior typing captures only local geometry and adds no function undo entries"
  (dolist (count '(1 50 200))
    (with-temp-buffer
      (dotimes (line 2000) (insert (format "line %d some text here\n" line)))
      (let (references)
        (dotimes (index count)
          (goto-char (point-min))
          (forward-line (* index 10))
          (let ((start (point)))
            (forward-line 3)
            (push (niwa-instruction-test--create-note-in (current-buffer) start (point)) references)))
        (let ((target (car (last references))))
          (goto-char (overlay-start target))
          (forward-line)
          (forward-char 5)
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (niwa--instruction-before-change (point) (point))
          (should (equal (list target)
                         (mapcar #'car (nth 2 niwa--pending-instruction-edit))))
          (dotimes (_ 10)
            (insert "x")
            (undo-boundary))
          (should-not (cl-find 'apply buffer-undo-list :key #'car-safe))
          (should (= 20 (length buffer-undo-list)))
          (undo-only 1)
          (should (= 1 (overlay-start target)))))))
  :doc "interior deletion needs no presentation snapshots or function undo entries"
  (with-temp-buffer
    (insert "before\nfirst line\nsecond line\nthird line\nafter\n")
    (let* ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 42))
           (start (overlay-start overlay))
           (end (overlay-end overlay)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region 12 18)
      (should-not (cl-find 'apply buffer-undo-list :key #'car-safe))
      (undo-boundary)
      (undo-only 1)
      (should (= start (overlay-start overlay)))
      (should (= end (overlay-end overlay))))))

(niwa-deftest niwa-instruction-undo-extend
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)
          (last-command nil))
   :quiet t)
  ,test
  (test)
  :doc "an interior edit promotes undo only when explicit mapping moves beyond native shifts"
  (dolist (mapped-start '(1 8))
    (dolist (strings '(("" . "x") ("x" . "x") ("XYZ" . "abc")))
      (with-temp-buffer
        (insert "before\nselected\nmore\nafter\n")
        (let* ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 22))
               (preceding (car strings))
               (appended (cdr strings))
               (mapped-end (+ 22 (length preceding) (length appended)))
               (history (list 10 nil)))
          (buffer-enable-undo)
          (setq buffer-undo-list history)
          (goto-char 10)
          ;; No boundary: native insertion undo can extend an existing interval.
          (insert preceding)
          (let ((niwa--instruction-line-mapping
                 (list (list overlay mapped-start mapped-end))))
            (insert appended))
          (should (= (if (= mapped-start 8) 0 2)
                     (cl-count 'apply buffer-undo-list :key #'car-safe)))
          (should (eq history (last buffer-undo-list 2)))
          (undo-boundary)
          (undo-only 1)
          (should (equal (buffer-string) "before\nselected\nmore\nafter\n"))
          (should (= 8 (overlay-start overlay)))
          (should (= 22 (overlay-end overlay)))
          (undo-boundary)
          (undo-redo 1)
          (should (equal (buffer-string)
                         (concat "before\nse" preceding appended "lected\nmore\nafter\n")))
          (should (= mapped-start (overlay-start overlay)))
          (should (= mapped-end (overlay-end overlay))))))))

(niwa-deftest niwa-instruction-undo--install
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)
          (last-command nil))
   :quiet t)
  ,test
  (test)
  :doc "full source deletion retains its note through cleanup, undo and redo"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 17)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region 8 17)
      (niwa--all-instructions)
      (should (eq (current-buffer) (overlay-buffer overlay)))
      (should (= 8 (overlay-start overlay) (overlay-end overlay)))
      (undo-boundary)
      (undo-only 1)
      (should (eq (current-buffer) (overlay-buffer overlay)))
      (should (= 8 (overlay-start overlay)))
      (should (= 17 (overlay-end overlay)))
      (undo-boundary)
      (undo-redo 1)
      (should (eq (current-buffer) (overlay-buffer overlay)))
      (should (= 8 (overlay-start overlay) (overlay-end overlay)))
      (undo-boundary)
      (undo-only 1)
      (should (= 17 (overlay-end overlay)))
      (should (memq overlay (niwa--instructions-in (point-min) (point-max))))))
  :doc "source deletion reserves note identity and reciprocal links through undo"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let* ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 17))
           (other (niwa-instruction-test--create-note-in (current-buffer) 18 20))
           (id (overlay-get overlay 'niwa-id)))
      (niwa-link-instructions (list id) (list (overlay-get other 'niwa-id)))
      (should (eq overlay (niwa--instruction-with-id id)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region 8 17)
      (niwa--all-instructions)
      (let ((new (niwa-instruction-test--create-note-in (current-buffer) 2 4)))
        (should-not (= id (overlay-get new 'niwa-id)))
        (undo-boundary)
        (undo-only 1)
        (should (= id (overlay-get overlay 'niwa-id)))
        (should (equal (list (overlay-get other 'niwa-id))
                       (niwa--instruction-outlinks overlay)))
        (should (equal (list (overlay-get overlay 'niwa-id))
                       (niwa--instruction-inlinks other)))
        (should (eq overlay (niwa--instruction-with-id id))))))
  :doc "source deletion keeps note identity while another note is stashed"
  (let* ((workspace (niwa-workspace--create
                     :type 'file :id "stash-undo" :root "/tmp" :name "stash-undo"))
         (file (make-temp-file "niwa-stashed-" nil ".txt" "other text\n"))
         (other-buffer (find-file-noselect file)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--workspace workspace)
          (insert "before\nselected\nafter\n")
          (let* ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 17))
                 (id (overlay-get overlay 'niwa-id)))
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            (delete-region 8 17)
            (niwa--all-instructions)
            (with-current-buffer other-buffer
              (setq-local niwa--workspace workspace)
              (let ((other (niwa-instruction-test--create-note-in other-buffer 2 5)))
                (should-not (= id (overlay-get other 'niwa-id))))
              (niwa--stash-buffer other-buffer))
            (undo-boundary)
            (undo-only 1)
            (should (= id (overlay-get overlay 'niwa-id)))))
      (with-current-buffer other-buffer
        (setq-local kill-buffer-hook nil)
        (set-buffer-modified-p nil))
      (kill-buffer other-buffer)
      (delete-file file)))
  :doc "restores links to a partner whose code was deleted in another buffer"
  (let* ((workspace (niwa-workspace--create
                     :type 'file :id "partner-undo" :root "/tmp" :name "partner-undo"))
         (cell (niwa-instruction-test--make-note "partner body\n" workspace))
         (partner (cdr cell)))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--workspace workspace)
          (insert "before\nselected\nafter\n")
          (let ((overlay (niwa-instruction-test--create-note-in (current-buffer) 8 17)))
            (niwa-link-instructions (list (overlay-get overlay 'niwa-id))
                                      (list (overlay-get partner 'niwa-id)))
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            (delete-region 8 17)
            (niwa--all-instructions)
            (with-current-buffer (car cell)
              (delete-region (overlay-start partner) (overlay-end partner)))
            (undo-boundary)
            (undo-only 1)
            (should (equal (list (overlay-get partner 'niwa-id))
                           (niwa--instruction-outlinks overlay)))
            (should (equal (list (overlay-get overlay 'niwa-id))
                           (niwa--instruction-inlinks partner)))))
      (niwa-instruction-test--discard cell)))
  :doc "rejected edits and disabled undo do not add presentation entries"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (niwa-instruction-test--create-note-in (current-buffer) 8 17)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((reject (lambda (&rest _) (error "Reject edit"))))
      (add-hook 'before-change-functions reject t t)
      (should-error (delete-region 8 17))
      (should-not buffer-undo-list)
      (remove-hook 'before-change-functions reject t))
    (buffer-disable-undo)
    (delete-region 8 17)
    (should (eq t buffer-undo-list)))
  :doc "restores nested note geometry after parent or nested-only deletion"
  (dolist (whole-tree '(nil t))
    ;; Each iteration owns its registry, as a killed buffer's notes stay listed.
    (let ((niwa--instruction-states (make-hash-table :test #'equal)))
      (with-temp-buffer
        (insert "outer child tail\n")
        (let* ((parent (niwa-instruction-test--create-note-in
                        (current-buffer) 1 17 'char))
               (child (niwa-instruction-test--create-note-in
                       (current-buffer) 7 12 'char))
               (child-id (overlay-get child 'niwa-id)))
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (delete-region (if whole-tree (overlay-start parent) 7)
                         (if whole-tree (overlay-end parent) 12))
          (niwa--all-instructions)
          (should (= (overlay-start child) (overlay-end child)))
          (undo-boundary)
          (undo-only 1)
          (should (eq (current-buffer) (overlay-buffer child)))
          (should (= 7 (overlay-start child)))
          (should (= 12 (overlay-end child)))
          (should (= child-id (overlay-get child 'niwa-id)))
          (should (= 2 (hash-table-count (niwa--instruction-id-usage-map))))
          (undo-boundary)
          (undo-redo 1)
          (should (eq (current-buffer) (overlay-buffer child)))
          (should (= (overlay-start child) (overlay-end child)))
          (undo-boundary)
          (undo-only 1)
          (should (= 7 (overlay-start child)))
          (should (= child-id (overlay-get child 'niwa-id)))
          (should (= 2 (hash-table-count (niwa--instruction-id-usage-map)))))))))

(provide 'test-niwa-instruction-undo)
;;; test-niwa-instruction-undo.el ends here
