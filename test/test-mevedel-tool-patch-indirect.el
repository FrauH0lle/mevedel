;;; test-mevedel-tool-patch-indirect.el --- Indirect patch transactions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise file transactions with native indirect buffers carrying the file
;; name of their base, as used by multi-mode editing packages.

;;; Code:

(require 'mevedel-tool-patch)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-tool-patch-indirect--buffer-state (buffer)
  "Read BUFFER's text, restriction, point and file bookkeeping for recovery."
  (with-current-buffer buffer
    (list (point-min) (point-max) (point) (buffer-modified-p)
          buffer-read-only buffer-file-name
          (unless (buffer-base-buffer) (visited-file-modtime))
          (save-restriction (widen) (buffer-string)))))

(mevedel-deftest mevedel-tool-patch-apply-indirect
  (:vars* ((configuration (current-window-configuration))
           (original-buffer (current-buffer))
           (root (make-temp-file "mevedel-patch-indirect-" t))
           (path (file-name-concat root "source.txt"))
           base child owned-buffers continued)
   :before-each
   (progn
     (with-temp-file path (insert "alpha\nold\nomega\n"))
     (push (setq base (find-file-noselect path)) owned-buffers)
     (push (setq child (make-indirect-buffer base
                                            (generate-new-buffer-name "patch-child")))
           owned-buffers)
     (with-current-buffer child
       (setq buffer-file-name (buffer-local-value 'buffer-file-name base))))
   :after-each
   (unwind-protect
       (let ((kill-buffer-query-functions nil))
         (dolist (buffer owned-buffers)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
         (when (file-directory-p root) (delete-directory root t)))
     (set-window-configuration configuration)
     (when (buffer-live-p original-buffer) (set-buffer original-buffer))))
  ,test
  (test)
  :doc "Applies and settles with the native file lookup returning an indirect child"
  (progn
    (switch-to-buffer child)
    (should (eq child (find-buffer-visiting path)))
    (mevedel-tool-patch-apply
     nil
     (mevedel-tool-patch-planned-changes
      (mevedel-tool-patch-parse
       "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"
       root))
     (lambda () (setq continued t)))
    (should continued)
    (should (equal "alpha\nnew\nomega\n" (mevedel-test--read-file path)))
    (dolist (buffer (list base child))
      (with-current-buffer buffer
        (should (equal "alpha\nnew\nomega\n" (buffer-string)))))
    (with-current-buffer base
      (should-not (buffer-modified-p))
      (should (verify-visited-file-modtime base))
      (should (time-equal-p (visited-file-modtime)
                           (file-attribute-modification-time
                            (file-attributes path))))))

  :doc "Base-selected control and narrowed siblings ignore current-buffer versus MRU order"
  (let ((second (make-indirect-buffer
                 base (generate-new-buffer-name "patch-second"))))
    (push second owned-buffers)
    (with-current-buffer second
      (setq buffer-file-name path)
      (narrow-to-region 1 6))
    (with-current-buffer child (narrow-to-region 7 10))
    (with-current-buffer base
      (narrow-to-region 1 10)
      (goto-char 3))
    (cl-loop
     for (selected current) in (list (list base child) (list child base)
                                    (list second child) (list base base))
     for index from 0
     for old = (if (zerop (% index 2)) "old" "new")
     for new = (if (zerop (% index 2)) "new" "old")
     do
     (switch-to-buffer selected)
     (with-current-buffer current
       (should (eq selected (find-buffer-visiting path)))
       (setq continued nil)
       (mevedel-tool-patch-apply
        nil
        (mevedel-tool-patch-planned-changes
         (mevedel-tool-patch-parse
          (format "*** Begin Patch\n*** Update File: source.txt\n@@\n-%s\n+%s\n*** End Patch"
                  old new)
          root))
        (lambda () (setq continued t))))
     (should continued)
     (should (equal (format "alpha\n%s\nomega\n" new)
                    (mevedel-test--read-file path)))
     (dolist (buffer (list base child second))
       (with-current-buffer buffer
         (save-restriction
           (widen)
           (should (equal (format "alpha\n%s\nomega\n" new) (buffer-string))))))
     (with-current-buffer base
       (should (= 1 (point-min)))
       (should (= 10 (point-max)))
       (should (= 3 (point)))
       (should-not (buffer-modified-p))
       (should (verify-visited-file-modtime base))))
    (with-current-buffer child
      (should (= 7 (point-min)))
      (should (= 10 (point-max)))))

  :doc "Alias lookup retains native base identity while a child is selected"
  (let ((alias (file-name-concat root "alias.txt")))
    (make-symbolic-link path alias)
    (kill-buffer child)
    (kill-buffer base)
    (push (setq base (find-file-noselect alias)) owned-buffers)
    (push (setq child (make-indirect-buffer
                      base (generate-new-buffer-name "patch-alias-child")))
          owned-buffers)
    (with-current-buffer child (setq buffer-file-name alias))
    (switch-to-buffer child)
    (should (eq child (find-buffer-visiting alias)))
    (should (eq base (find-buffer-visiting path)))
    (mevedel-tool-patch-apply
     nil
     (mevedel-tool-patch-planned-changes
      (mevedel-tool-patch-parse
       "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch"
       root))
     (lambda () (setq continued t)))
    (should continued)
    (should (equal "alpha\nnew\nomega\n" (mevedel-test--read-file alias)))
    (dolist (buffer (list base child))
      (with-current-buffer buffer
        (should (equal "alpha\nnew\nomega\n" (buffer-string)))))
    (should-not (buffer-modified-p base))
    (should (verify-visited-file-modtime base)))

  :doc "External replacement and children recreated after revert remain safe"
  (progn
    (let ((replacement (file-name-concat root "replacement.txt")))
      (with-temp-file replacement (insert "alpha\nexternal\nomega\n"))
      (set-file-times replacement
                      (time-add (with-current-buffer base (visited-file-modtime)) 2))
      (rename-file replacement path t))
    (should-not (verify-visited-file-modtime base))
    (switch-to-buffer child)
    (should (eq child (find-buffer-visiting path)))
    (mevedel-tool-patch-apply
     nil
     (mevedel-tool-patch-planned-changes
      (mevedel-tool-patch-parse
       "*** Begin Patch\n*** Update File: source.txt\n@@\n-external\n+new\n*** End Patch"
       root))
     (lambda () (setq continued t)))
    (should continued)
    (should (equal "alpha\nnew\nomega\n" (mevedel-test--read-file path)))
    (dolist (buffer (list base child))
      (with-current-buffer buffer
        (should (equal "alpha\nnew\nomega\n" (buffer-string)))))
    (should-not (buffer-modified-p base))
    (should (verify-visited-file-modtime base))
    (kill-buffer child)
    (with-temp-file path (insert "alpha\nreverted\nomega\n"))
    (with-current-buffer base (revert-buffer t t t))
    (push (setq child (make-indirect-buffer
                      base (generate-new-buffer-name "patch-recreated")))
          owned-buffers)
    (with-current-buffer child (setq buffer-file-name path))
    (setq continued nil)
    (switch-to-buffer child)
    (should (eq child (find-buffer-visiting path)))
    (mevedel-tool-patch-apply
     nil
     (mevedel-tool-patch-planned-changes
      (mevedel-tool-patch-parse
       "*** Begin Patch\n*** Update File: source.txt\n@@\n-reverted\n+new\n*** End Patch"
       root))
     (lambda () (setq continued t)))
    (should continued)
    (should (equal "alpha\nnew\nomega\n" (mevedel-test--read-file path)))
    (dolist (buffer (list base child))
      (with-current-buffer buffer
        (should (equal "alpha\nnew\nomega\n" (buffer-string)))))
    (should-not (buffer-modified-p base))
    (should (verify-visited-file-modtime base)))

  :doc "Unsaved or read-only bases reject every target before writes despite writable children"
  (let* ((other (file-name-concat root "other.txt"))
         (modtime (with-current-buffer base (visited-file-modtime))))
    (with-temp-file other (insert "untouched\n"))
    (dolist (guard '(unsaved read-only))
      (with-current-buffer base
        (if (eq guard 'unsaved)
            (progn (goto-char (point-max)) (insert "user draft\n"))
          (setq buffer-read-only t)))
      (with-current-buffer child (setq buffer-read-only nil))
      (switch-to-buffer child)
      (should (eq child (find-buffer-visiting path)))
      (let* ((disk-before
              (mapcar (lambda (target)
                        (let ((attributes (file-attributes target)))
                          (list target
                                (file-attribute-modification-time attributes)
                                (file-attribute-inode-number attributes))))
                      (list other path)))
             (changes
              (mevedel-tool-patch-planned-changes
               (mevedel-tool-patch-parse
                (concat "*** Begin Patch\n*** Update File: other.txt\n@@\n-untouched\n+changed\n"
                        "*** Update File: source.txt\n@@\n-old\n+new\n*** End Patch")
                root)))
             (failure
              (should-error
               (mevedel-tool-patch-apply
                nil changes (lambda () (setq continued t)))
               :type 'error)))
        (should (string-match-p (if (eq guard 'unsaved) "unsaved" "read-only")
                                (error-message-string failure)))
        (dolist (entry disk-before)
          (let ((attributes (file-attributes (car entry))))
            (should (equal (nth 1 entry)
                           (file-attribute-modification-time attributes)))
            (should (equal (nth 2 entry)
                           (file-attribute-inode-number attributes))))))
      (should-not continued)
      (should (equal "untouched\n" (mevedel-test--read-file other)))
      (should (equal "alpha\nold\nomega\n" (mevedel-test--read-file path)))
      (dolist (buffer (list base child))
        (with-current-buffer buffer
          (should (equal (if (eq guard 'unsaved)
                             "alpha\nold\nomega\nuser draft\n"
                           "alpha\nold\nomega\n")
                         (buffer-string)))))
      (with-current-buffer base
        (should (eq (eq guard 'unsaved) (and (buffer-modified-p) t)))
        (should (equal modtime (visited-file-modtime)))
        (setq buffer-read-only nil)
        (when (eq guard 'unsaved)
          (delete-region 17 (point-max))
          (set-buffer-modified-p nil)))))

  :doc "Delete and move clear the base's visited modtime without killing its children"
  (let ((destination (file-name-concat root "moved.txt")))
    (dolist (operation '(delete move))
      (with-temp-file path (insert "alpha\nold\nomega\n"))
      (setq continued nil)
      (switch-to-buffer child)
      (should (eq child (find-buffer-visiting path)))
      (mevedel-tool-patch-apply
       nil
       (mevedel-tool-patch-planned-changes
        (mevedel-tool-patch-parse
         (if (eq operation 'delete)
             "*** Begin Patch\n*** Delete File: source.txt\n*** End Patch"
           (concat "*** Begin Patch\n*** Update File: source.txt\n"
                   "*** Move to: moved.txt\n@@\n-old\n+new\n*** End Patch"))
         root))
       (lambda () (setq continued t)))
      (should continued)
      (should-not (file-exists-p path))
      (if (eq operation 'delete)
          (should-not (file-exists-p destination))
        (should (equal "alpha\nnew\nomega\n"
                       (mevedel-test--read-file destination))))
      (dolist (buffer (list base child))
        (should (buffer-live-p buffer))
        (with-current-buffer buffer
          (should (equal "alpha\nold\nomega\n" (buffer-string)))))
      (with-current-buffer base
        (should (equal path buffer-file-name))
        (should-not (buffer-modified-p))
        (should (equal 0 (visited-file-modtime))))))

  :doc "Public commit restores exact base state after a confirmed post-write sync failure"
  (let* ((other (file-name-concat root "other.txt"))
         (overlay (make-overlay 7 10 base))
         second second-child states sync-failure-reached writes-observed)
    (with-temp-file other (insert "before\nold two\nafter\n"))
    (set-file-modes path #o751)
    (set-file-modes other #o640)
    (push (setq second (find-file-noselect other)) owned-buffers)
    (push (setq second-child
                (make-indirect-buffer second (generate-new-buffer-name "patch-rollback")))
          owned-buffers)
    (with-current-buffer second-child
      (setq buffer-file-name other)
      (narrow-to-region 1 7)
      (goto-char 3))
    (with-current-buffer child
      (narrow-to-region 1 6)
      (goto-char 3))
    (with-current-buffer base
      (narrow-to-region 1 10)
      (goto-char 8))
    (overlay-put overlay 'patch-test 'original)
    (switch-to-buffer child)
    (switch-to-buffer second-child)
    (should (eq child (find-buffer-visiting path)))
    (should (eq second-child (find-buffer-visiting other)))
    (with-current-buffer second
      (add-hook
       'before-change-functions
       (lambda (&rest _)
         (setq sync-failure-reached t
               writes-observed
               (list (mevedel-test--read-file path)
                     (mevedel-test--read-file other)
                     (with-current-buffer base
                       (save-restriction (widen) (buffer-string)))))
         (error "Injected indirect transaction sync failure"))
       nil t))
    (setq states (mapcar #'test-mevedel-tool-patch-indirect--buffer-state
                         (list base child second second-child)))
    (let ((failure
           (should-error
            (mevedel-tool-patch-commit
             (mevedel-tool-patch-planned-changes
              (mevedel-tool-patch-parse
               (concat "*** Begin Patch\n*** Update File: source.txt\n@@\n-old\n+new\n"
                       "*** Update File: other.txt\n@@\n-old two\n+new two\n*** End Patch")
               root)))
            :type 'error)))
      (should sync-failure-reached)
      (should (equal '("alpha\nnew\nomega\n" "before\nnew two\nafter\n"
                       "alpha\nnew\nomega\n")
                     writes-observed))
      (should (equal '(error "Injected indirect transaction sync failure") failure)))
    (dolist (entry (list (cons path "alpha\nold\nomega\n")
                        (cons other "before\nold two\nafter\n")))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally (car entry))
        (should (equal (cdr entry) (buffer-string)))))
    (should (= #o751 (file-modes path)))
    (should (= #o640 (file-modes other)))
    (should (equal states
                   (mapcar #'test-mevedel-tool-patch-indirect--buffer-state
                           (list base child second second-child))))
    (should (eq base (overlay-buffer overlay)))
    (should (= 7 (overlay-start overlay)))
    (should (= 10 (overlay-end overlay)))
    (should (eq 'original (overlay-get overlay 'patch-test)))))

(provide 'test-mevedel-tool-patch-indirect)
;;; test-mevedel-tool-patch-indirect.el ends here
