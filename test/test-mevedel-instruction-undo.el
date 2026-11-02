;;; test-mevedel-instruction-undo.el --- Source deletion undo tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise native undo/redo, including registry cleanup and directive history.

;;; Code:

(require 'mevedel-directive-source)
(require 'mevedel-overlays)
(require 'mevedel-instruction-undo)
(require 'mevedel-directive)
(require 'mevedel-directive-request)
(require 'mevedel-persistence)
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

(mevedel-deftest mevedel-instruction-undo-capture
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)
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
            (push (mevedel--create-reference-in (current-buffer) start (point)) references)))
        (let ((target (car (last references))))
          (goto-char (overlay-start target))
          (forward-line)
          (forward-char 5)
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (mevedel--instruction-before-change (point) (point))
          (should (equal (list target)
                         (mapcar #'car (nth 2 mevedel--pending-instruction-edit))))
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
    (let* ((overlay (mevedel--create-reference-in (current-buffer) 8 42))
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

(mevedel-deftest mevedel-instruction-undo-extend
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)
          (last-command nil))
   :quiet t)
  ,test
  (test)
  :doc "an interior edit promotes undo only when explicit mapping moves beyond native shifts"
  (dolist (mapped-start '(1 8))
    (dolist (strings '(("" . "x") ("x" . "x") ("XYZ" . "abc")))
      (with-temp-buffer
        (insert "before\nselected\nmore\nafter\n")
        (let* ((overlay (mevedel--create-reference-in (current-buffer) 8 22))
               (preceding (car strings))
               (appended (cdr strings))
               (mapped-end (+ 22 (length preceding) (length appended)))
               (history (list 10 nil)))
          (buffer-enable-undo)
          (setq buffer-undo-list history)
          (goto-char 10)
          ;; No boundary: native insertion undo can extend an existing interval.
          (insert preceding)
          (let ((mevedel--instruction-line-mapping
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

(defun mevedel-instruction-test--undo-settled-details (ordering outcome)
  "Probe source undo across terminal OUTCOME in ORDERING."
  (let* ((mevedel--instruction-states (make-hash-table :test #'equal))
         (mevedel--instruction-current-state-key :global)
         (workspace (mevedel-workspace--create
                     :type 'file :id "consumed-undo" :root "/tmp" :name "undo"))
         (cell (mevedel-instruction-test--make-directive
                "outer child tail\n" "Parent" workspace))
         (parent (cdr cell))
         (record (mevedel--directive-record parent)))
    (unwind-protect
        (with-current-buffer (car cell)
          (let* ((child (mevedel--create-directive-in
                         (current-buffer) 7 12 nil "Submitted detail"))
                 (child-id (overlay-get child 'mevedel-uuid))
                 (submitted (mevedel--submitted-subdirectives parent))
                 ;; A detail authored after dispatch must remain pending.
                 (later (mevedel--create-directive-in
                         (current-buffer) 13 16 nil "Later detail"))
                 (later-record (mevedel--subdirective-record later))
                 (text (buffer-string)))
            (mevedel--record-directive-terminal-activity
             record 'discuss "Parent" "question" "answer" 'success
             '(:session-id "session" :turn 1) nil nil nil)
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            ;; An older geometry snapshot must not revive consumed details
            ;; either, even when the deletion itself happens after success.
            (when (eq ordering 'edit-complete-delete)
              (goto-char 7)
              (insert "x")
              (undo-boundary))
            (cl-labels
                ((complete ()
                   (mevedel--record-directive-terminal-activity
                    record 'implement "Parent" "prompt" "result" outcome
                    '(:session-id "session" :turn 2)
                    '(:mevedel-directive-capture complete) nil submitted)
                   (mevedel--settle-directive-presentation
                    (mevedel--instruction-with-uuid
                     (mevedel-directive-id record) workspace)
                    record workspace t submitted
                    (unless (eq outcome 'success) outcome))))
              (unless (eq ordering 'delete-complete) (complete))
              (delete-region (overlay-start parent) (overlay-end parent))
              (mevedel--all-instructions)
              (when (eq ordering 'delete-complete) (complete)))
            (let* ((attempts (mevedel-directive-attempts record))
                   (discussion (mevedel-directive-discussion record))
                   (history (copy-tree (list attempts discussion
                                            (mevedel-directive-planning record)) t))
                   (attempt (car attempts))
                   (consumed (eq outcome 'success)))
              (should (eq outcome (mevedel-directive-attempt-outcome attempt)))
              (should (equal (and consumed (list child-id))
                             (mapcar #'mevedel-subdirective-id
                                     (mevedel-directive-attempt-consumed-subdirectives
                                      attempt))))
              (should (eq (not consumed)
                          (not (null (cl-find child-id
                                              (mevedel-directive-subdirectives record)
                                              :key #'mevedel-subdirective-id
                                              :test #'equal)))))
              (dotimes (_ 2)
                (undo-boundary)
                (undo-only 1)
                (should (eq parent (mevedel--instruction-with-uuid
                                   (mevedel-directive-id record) workspace)))
                (should (eq 'attached
                            (plist-get (mevedel-directive-anchor record) :state)))
                (should (eq (current-buffer) (overlay-buffer later)))
                (should (memq later-record (mevedel-directive-subdirectives record)))
                (should (equal (if consumed (list (mevedel-subdirective-id later-record))
                                 (list child-id (mevedel-subdirective-id later-record)))
                               (mapcar #'mevedel-subdirective-id
                                       (mevedel--submitted-subdirectives parent))))
                (should (eq (not consumed) (not (null (overlay-buffer child)))))
                (should (= 1 (overlay-start parent)))
                (should (= (1- (point-max)) (overlay-end parent)))
                (should (= (if (eq ordering 'edit-complete-delete) 14 13)
                           (overlay-start later)))
                (unless consumed
                  (should (= 7 (overlay-start child)))
                  (should (= 12 (overlay-end child))))
                (should (eq attempts (mevedel-directive-attempts record)))
                (should (eq discussion (mevedel-directive-discussion record)))
                (should (equal history (list attempts discussion
                                             (mevedel-directive-planning record))))
                (undo-boundary)
                (undo-redo 1))
              (undo-boundary)
              (undo-only 1)
              (when (eq ordering 'edit-complete-delete)
                (undo-boundary)
                (undo-only 1)
                (should-not (overlay-buffer child))
                (should (equal history (list (mevedel-directive-attempts record)
                                             (mevedel-directive-discussion record)
                                             (mevedel-directive-planning record))))))
            (should (equal text (buffer-string)))))
      (mevedel-instruction-test--discard cell))))

(mevedel-deftest mevedel-instruction-undo--install
  (:vars ((mevedel--instruction-states (make-hash-table :test #'equal))
          (mevedel--instruction-current-state-key :global)
          (last-command nil))
   :quiet t)
  ,test
  (test)
  :doc "does not resurrect details consumed after deletion; failure and abort restore them"
  (dolist (outcome '(error aborted success))
    (mevedel-instruction-test--undo-settled-details 'delete-complete outcome))
  :doc "completion before deletion keeps consumed details absent and later details pending"
  (dolist (outcome '(error aborted success))
    (mevedel-instruction-test--undo-settled-details 'complete-delete outcome))
  :doc "older geometry undo cannot resurrect a detail consumed before deletion"
  (mevedel-instruction-test--undo-settled-details 'edit-complete-delete 'success)
  :doc "full reference deletion survives cleanup, undo, redo and another undo"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let ((overlay (mevedel--create-reference-in (current-buffer) 8 17)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region 8 17)
      (mevedel--all-instructions)
      (should-not (overlay-buffer overlay))
      (undo-boundary)
      (undo-only 1)
      (should (eq (current-buffer) (overlay-buffer overlay)))
      (should (= 8 (overlay-start overlay)))
      (should (= 17 (overlay-end overlay)))
      (undo-boundary)
      (undo-redo 1)
      (should-not (overlay-buffer overlay))
      (undo-boundary)
      (undo-only 1)
      (should (= 17 (overlay-end overlay)))
      (should (memq overlay (mevedel--instructions-in (point-min) (point-max))))))
  :doc "restores reciprocal links when a deleted reference ID was reused"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let* ((overlay (mevedel--create-reference-in (current-buffer) 8 17))
           (other (mevedel--create-reference-in (current-buffer) 18 20))
           (id (overlay-get overlay 'mevedel-id)))
      (mevedel-link-instructions (list id) (list (overlay-get other 'mevedel-id)))
      (should (eq overlay (mevedel--instruction-with-id id)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region 8 17)
      (mevedel--all-instructions)
      (let ((new (mevedel--create-reference-in (current-buffer) 2 4)))
        (should (= id (overlay-get new 'mevedel-id)))
        (undo-boundary)
        (undo-only 1)
        (should-not (= id (overlay-get overlay 'mevedel-id)))
        (should (equal (list (overlay-get other 'mevedel-id))
                       (mevedel--instruction-outlinks overlay)))
        (should (equal (list (overlay-get overlay 'mevedel-id))
                       (mevedel--instruction-inlinks other)))
        (should (eq new (mevedel--instruction-with-id id))))))
  :doc "re-reserves an available ID after bulk deletion retires its temporary owner"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (let* ((overlay (mevedel--create-reference-in (current-buffer) 8 17))
           (id (overlay-get overlay 'mevedel-id)))
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (delete-region 8 17)
      (mevedel--all-instructions)
      (let ((temporary (mevedel--create-reference-in (current-buffer) 2 4)))
        (should (= id (overlay-get temporary 'mevedel-id))))
      (mevedel-delete-all-references)
      (should-not (gethash id (mevedel--instruction-id-usage-map)))
      (undo-boundary)
      (undo-only 1)
      (should (= id (overlay-get overlay 'mevedel-id)))
      (should (gethash id (mevedel--instruction-id-usage-map)))
      (should-not (memq id (mevedel--instruction-retired-ids)))
      (should (equal (list overlay) (mevedel--all-instructions)))))
  :doc "does not reclaim an ID used by a stashed reference"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "stash-undo" :root "/tmp" :name "stash-undo"))
         (file (make-temp-file "mevedel-stashed-" nil ".txt" "other text\n"))
         (other-buffer (find-file-noselect file)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--workspace workspace)
          (insert "before\nselected\nafter\n")
          (let* ((overlay (mevedel--create-reference-in (current-buffer) 8 17))
                 (id (overlay-get overlay 'mevedel-id)))
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            (delete-region 8 17)
            (mevedel--all-instructions)
            (with-current-buffer other-buffer
              (setq-local mevedel--workspace workspace)
              (let ((other (mevedel--create-reference-in other-buffer 2 5)))
                (should (= id (overlay-get other 'mevedel-id))))
              (mevedel--stash-buffer other-buffer))
            (undo-boundary)
            (undo-only 1)
            (should-not (= id (overlay-get overlay 'mevedel-id)))))
      (with-current-buffer other-buffer
        (setq-local kill-buffer-hook nil)
        (set-buffer-modified-p nil))
      (kill-buffer other-buffer)
      (delete-file file)))
  :doc "restores links to a partner whose presentation detached in another buffer"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "partner-undo" :root "/tmp" :name "partner-undo"))
         (cell (mevedel-instruction-test--make-directive
                "partner body\n" "Partner" workspace))
         (directive (cdr cell))
         (uuid (overlay-get directive 'mevedel-uuid)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--workspace workspace)
          (insert "before\nselected\nafter\n")
          (let ((overlay (mevedel--create-reference-in (current-buffer) 8 17)))
            (mevedel-link-instructions (list (overlay-get overlay 'mevedel-id))
                                      (list (overlay-get directive 'mevedel-id)))
            (buffer-enable-undo)
            (setq buffer-undo-list nil)
            (delete-region 8 17)
            (mevedel--all-instructions)
            (with-current-buffer (car cell)
              (delete-region (overlay-start directive) (overlay-end directive)))
            (undo-boundary)
            (undo-only 1)
            (let ((partner (mevedel--instruction-with-uuid uuid workspace)))
              (should (equal (list (overlay-get partner 'mevedel-id))
                             (mevedel--instruction-outlinks overlay)))
              (should (equal (list (overlay-get overlay 'mevedel-id))
                             (mevedel--instruction-inlinks partner))))))
      (mevedel-instruction-test--discard cell)))
  :doc "rejected edits and disabled undo do not add presentation entries"
  (with-temp-buffer
    (insert "before\nselected\nafter\n")
    (mevedel--create-reference-in (current-buffer) 8 17)
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
  :doc "restores nested source presentations after parent or nested-only deletion"
  (dolist (whole-tree '(nil t))
    (let* ((workspace (mevedel-workspace--create
                       :type 'file :id (format "tree-undo-%s" whole-tree)
                       :root "/tmp" :name "tree-undo"))
           (cell (mevedel-instruction-test--make-directive
                  "outer child tail\n" "Parent" workspace))
           (parent (cdr cell))
           (record (mevedel--directive-record parent)))
      (unwind-protect
          (with-current-buffer (car cell)
            (let* ((child (mevedel--create-directive-in
                           (current-buffer) 7 12 nil "Child"))
                   (child-id (overlay-get child 'mevedel-id))
                   (nested (mevedel--subdirective-record child)))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (delete-region (if whole-tree (overlay-start parent) 7)
                             (if whole-tree (overlay-end parent) 12))
              (mevedel--all-instructions)
              (should-not (overlay-buffer child))
              (undo-boundary)
              (undo-only 1)
              (should (eq (current-buffer) (overlay-buffer child)))
              (should (= 7 (overlay-start child)))
              (should (= 12 (overlay-end child)))
              (should (= child-id (overlay-get child 'mevedel-id)))
              (should (= 2 (hash-table-count (mevedel--instruction-id-usage-map))))
              (should (equal (list nested) (mevedel-directive-subdirectives record)))
              (undo-boundary)
              (undo-redo 1)
              (should-not (overlay-buffer child))
              (should (= (if whole-tree 1 0)
                         (length (mevedel-directive-subdirectives record))))
              (undo-boundary)
              (undo-only 1)
              (should (eq nested (mevedel--subdirective-record child)))
              (should (= child-id (overlay-get child 'mevedel-id)))
              (should (= 2 (hash-table-count (mevedel--instruction-id-usage-map))))))
        (mevedel-instruction-test--discard cell))))
  :doc "directive undo restores the original overlay and keeps newer history"
  (let* ((workspace (mevedel-workspace--create
                     :type 'file :id "undo" :root "/tmp" :name "undo"))
         (cell (mevedel-instruction-test--make-directive
                "before\nselected\nafter\n" "Keep history" workspace))
         (overlay (cdr cell))
         (record (mevedel--directive-record overlay))
         (start (overlay-start overlay))
         (end (overlay-end overlay))
         reference)
    (unwind-protect
        (with-current-buffer (car cell)
          (setq reference (mevedel--create-reference-in (current-buffer) end (point-max)))
          (mevedel-link-instructions (list (overlay-get reference 'mevedel-id))
                                    (list (overlay-get overlay 'mevedel-id)))
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (delete-region start end)
          (setf (mevedel-directive-attempts record)
                (list (mevedel-instruction-test--attempt)))
          (undo-boundary)
          (undo-only 1)
          (should (eq overlay (mevedel--instruction-with-uuid
                               (mevedel-directive-id record) workspace)))
          (should (eq 'attached (plist-get (mevedel-directive-anchor record) :state)))
          (should (equal (list (overlay-get reference 'mevedel-id))
                         (mevedel--instruction-inlinks overlay)))
          (should (= 1 (length (mevedel-directive-attempts record))))
          (undo-boundary)
          (undo-redo 1)
          (should (eq 'detached (plist-get (mevedel-directive-anchor record) :state)))
          (should (equal (list (overlay-get reference 'mevedel-id))
                         (mevedel--instruction-inlinks overlay)))
          (undo-boundary)
          (undo-only 1)
          (should (= start (overlay-start overlay)))
          (should (= end (overlay-end overlay)))
          (should (= 1 (length (mevedel-directive-attempts record)))))
      (mevedel-instruction-test--discard cell))))

(provide 'test-mevedel-instruction-undo)
;;; test-mevedel-instruction-undo.el ends here
