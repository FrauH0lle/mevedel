;;; test-mevedel-patch-review.el --- Tests for the ApplyPatch review UI -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for the staged ApplyPatch review interface.

;;; Code:

(require 'mevedel)
(require 'mevedel-tool-patch)
(require 'mevedel-patch-review)
(require 'mevedel-view)
(require 'mevedel-view-interaction)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
            "helpers"))
(require 'mevedel-session-persistence)

(defvar mevedel-session--read-only-mode nil)

(mevedel-deftest mevedel-patch-review-start
  (:doc "One hierarchical review stages hunk selection before aggregate apply")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-review-" t)))
           (one (file-name-concat root "one.txt"))
           (two (file-name-concat root "two.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "review"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "review" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@ a"
                     "-old1"
                     "+new1"
                     "@@ middle"
                     "-old2"
                     "+new2"
                     "*** Update File: two.txt"
                     "@@"
                     "-before"
                     "+after"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file one
              (insert "a\nold1\nmiddle\nold2\nz\n"))
            (with-temp-file two (insert "before\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (should-not result)
            (should (equal "a\nold1\nmiddle\nold2\nz\n"
                           (with-temp-buffer
                             (insert-file-contents one)
                             (buffer-string))))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search
                         "ApplyPatch · 2 files · 3/3 changes · +3 −3" text))
                (should (string-search "M one.txt" text))
                (should (string-search "M two.txt" text))
                ;; Every file starts folded.
                (should-not (string-search "@@ a" text))
                (should-not (string-search "│ old1" text))
                (should-not (string-search "│ before" text)))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search "- │ old1" text))
                (should (string-search "@@ a" text))
                (should-not (string-search "│ before" text)))
              (goto-char (point-min))
              (search-forward "@@ a")
              (mevedel-patch-review-toggle-fold)
              (should-not (string-search
                           "- │ old1"
                           (buffer-substring-no-properties
                            (point-min) mevedel-view--input-marker)))
              (goto-char (point-min))
              (search-forward "@@ a")
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ a")
              (mevedel-patch-review-toggle-selection)
              (should (string-search
                       "ApplyPatch · 2 files · 2/3 changes · +2 −2"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit))
            (should (equal "a\nold1\nmiddle\nnew2\nz\n"
                           (with-temp-buffer
                             (insert-file-contents one)
                             (buffer-string))))
            (should (equal "after\n"
                           (with-temp-buffer
                             (insert-file-contents two)
                             (buffer-string))))
            (should (string-search "Rejected: one.txt hunk 1"
                                   (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-feedback
  (:doc "Per-hunk multiline feedback stages independent revision notes")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-feedback-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "feedback"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "feedback" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@ first"
                     "-old1"
                     "+new1"
                     "@@ second"
                     "-old2"
                     "+new2"
                     "*** End Patch")
                   "\n"))
           result proposal)
      (unwind-protect
          (progn
            (with-temp-file path (insert "first\nold1\nsecond\nold2\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (should (get-text-property (1- (point)) 'read-only))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ first")
              (setq proposal (get-text-property
                              (1- (point)) 'mevedel-patch-proposal))
              (mevedel-patch-review-feedback)
              (should-not (get-text-property (point) 'read-only))
              (should (string-search
                       "C-c C-c stage feedback"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
              (insert "unfinished draft")
              (mevedel-patch-review--render proposal)
              (should (string-search
                       "unfinished draft"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
              ;; Folding the file must keep the live feedback editor
              ;; rendered so its draft and markers survive redraws.
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (should (string-search
                       "unfinished draft"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
              (should (text-property-any
                       (point-min) (point-max)
                       'mevedel-patch-feedback-input
                       (car (plist-get
                             (car (plist-get proposal :operations))
                             :hunks))))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (goto-char (text-property-any
                          (point-min) (point-max)
                          'mevedel-patch-feedback-input
                          (car (plist-get
                                (car (plist-get proposal :operations))
                                :hunks))))
              (mevedel-patch-review-cancel-feedback)
              (goto-char (point-min))
              (search-forward "✓ @@ first")
              (mevedel-patch-review-feedback)
              (insert "Keep the old public behavior.\nIt is documented.")
              (mevedel-patch-review-confirm-feedback)
              (goto-char (point-min))
              (search-forward "@@ first")
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) nil)))
                (should-error (mevedel-patch-review-edit-hunk)
                              :type 'user-error))
              (goto-char (point-min))
              (search-forward "@@ second")
              (mevedel-patch-review-feedback)
              (insert "Use the newer API instead.")
              (mevedel-patch-review-confirm-feedback)
              ;; File-level feedback on an Update file keeps hunk
              ;; selection untouched.
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-feedback)
              (insert "Keep this file minimal.")
              (mevedel-patch-review-confirm-feedback)
              ;; Patch-level feedback from the header.
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-feedback)
              (insert "Overall: split this into two patches.")
              (mevedel-patch-review-confirm-feedback)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search "[ Request revision · 4 comments ]" text))
                (should (string-search "Keep the old public behavior." text))
                (should (string-search "Use the newer API instead." text))
                (should (string-search "Keep this file minimal." text))
                (should (string-search "Overall: split this into two patches."
                                       text)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit))
            (should (equal "first\nold1\nsecond\nold2\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))))
            (should (string-search
                     "Feedback (whole patch): Overall: split this into two patches."
                     (plist-get result :result)))
            (should (string-search
                     "Feedback: one.txt: Keep this file minimal."
                     (plist-get result :result)))
            (should (string-search
                     "Feedback: one.txt hunk 1: Keep the old public behavior."
                     (plist-get result :result)))
            (should (string-search
                     "Feedback: one.txt hunk 2: Use the newer API instead."
                     (plist-get result :result)))
            (should (member
                     "Feedback: one.txt hunk 2: Use the newer API instead."
                     (plist-get (plist-get result :render-data) :notes))))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-toggle-selection
  (:doc "Add, Delete, and Move remain whole-operation review choices")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-whole-" t)))
           (delete-path (file-name-concat root "delete.txt"))
           (move-path (file-name-concat root "move.txt"))
           (moved-path (file-name-concat root "moved.txt"))
           (added-path (file-name-concat root "new.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "whole"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "whole" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: new.txt"
                     "+added"
                     "*** Delete File: delete.txt"
                     "*** Update File: move.txt"
                     "*** Move to: moved.txt"
                     "@@"
                     "-old"
                     "+moved"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file delete-path (insert "keep\n"))
            (with-temp-file move-path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "D delete.txt")
              (mevedel-patch-review-toggle-selection)
              (goto-char (point-min))
              (search-forward "R move.txt → moved.txt")
              (mevedel-patch-review-toggle-fold)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search "- │ old" text))
                ;; Move diffs stay whole-operation: no selectable hunk rows.
                (should-not (string-search "@@" text))
                (should-not
                 (or (get-text-property (point-min) 'mevedel-patch-hunk)
                     (next-single-property-change (point-min)
                                                  'mevedel-patch-hunk))))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit))
            (should (file-exists-p delete-path))
            (should (equal "added\n"
                           (with-temp-buffer
                             (insert-file-contents added-path)
                             (buffer-string))))
            (should-not (file-exists-p move-path))
            (should (equal "moved\n"
                           (with-temp-buffer
                             (insert-file-contents moved-path)
                             (buffer-string))))
            (should (string-search "Rejected: delete.txt"
                                   (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "SPC on an Update file row toggles every hunk in the file"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-filewide-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "filewide"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "filewide" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@ first"
                     "-old1"
                     "+new1"
                     "@@ second"
                     "-old2"
                     "+new2"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "first\nold1\nsecond\nold2\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-selection)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search "0/2 changes" text))
                (should (string-search "✗ @@ first" text))
                (should (string-search "✗ @@ second" text))
                (should (string-search "[ Reject patch ]" text)))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-selection)
              (should (string-search
                       "2/2 changes"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit))
            (should (equal "first\nnew1\nsecond\nnew2\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-edit-hunk
  (:doc "Editing one hunk replaces and marks the staged model proposal")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-edit-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "edit"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "edit" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+model"
                     "*** End Patch")
                   "\n"))
           result edit-buffer)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ old")
              (setq edit-buffer (mevedel-patch-review-edit-hunk)))
            (with-current-buffer edit-buffer
              (erase-buffer)
              (insert "@@\n-old\n+user edited\n")
              (mevedel-patch-review-edit-commit))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search "+ │ user edited" text))
                (should (string-search "· edited" text)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit))
            (should (equal "user edited\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))))
            (should (string-search "Modified (authoritative): one.txt hunk 1"
                                   (plist-get result :result))))
        (when (buffer-live-p edit-buffer) (kill-buffer edit-buffer))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-submit
  (:doc "A stale selected file writes nothing and leaves review recoverable")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-stale-" t)))
           (one (file-name-concat root "one.txt"))
           (two (file-name-concat root "two.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "stale"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "stale" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old one"
                     "+new one"
                     "*** Update File: two.txt"
                     "@@"
                     "-old two"
                     "+new two"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file one (insert "old one\n"))
            (with-temp-file two (insert "old two\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-temp-file two (insert "external change\n"))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit)
              (should-not result)
              (should (string-search
                       "Conflict: File changed during review: two.txt"
                       (buffer-substring-no-properties
                        (point-min) mevedel-view--input-marker))))
            (should (equal "old one\n"
                           (with-temp-buffer
                             (insert-file-contents one)
                             (buffer-string))))
            (should (equal "external change\n"
                           (with-temp-buffer
                             (insert-file-contents two)
                             (buffer-string)))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "RET on Apply selected writes the patch and settles the callback"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-submit-key-" t)))
           (path (file-name-concat root "test.el"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "submit-key"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "submit-key" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: test.el"
                     "@@"
                     "-(defun hello-world ())"
                     "+(defun hello-world ()"
                     "+  (interactive)"
                     "+  (message \"Hello, world!\"))"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "(defun hello-world ())\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "[ Apply 1 change in 1 file ]")
              (backward-char 2)
              (let ((command (key-binding (kbd "RET"))))
                (should (eq command #'mevedel-patch-review-submit))
                (call-interactively command)))
            (should result)
            (should (equal "(defun hello-world ()\n  (interactive)\n  (message \"Hello, world!\"))\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A newly active parent is disclosed before the patch can apply"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-parent-race-" t)))
           (path (file-name-concat root "one.txt"))
           (parent-data (generate-new-buffer " *mevedel-patch-parent*"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "parent-race"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "parent-race" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+new"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer parent-data
              (setq-local mevedel--current-request nil))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session
                          mevedel-side-conversation--parent-buffer parent-data)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (should-not
               (string-search
                "parent request is still active"
                (buffer-substring-no-properties
                 (point-min) mevedel-view--input-marker))))
            (with-current-buffer parent-data
              (setq-local mevedel--current-request
                  (mevedel-request--create :session mevedel--session)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit)
              (should-not result)
              (should
               (string-search
                "parent request is still active"
                (buffer-substring-no-properties
                 (point-min) mevedel-view--input-marker)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit))
            (should result)
            (should (equal "new\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
        (when (buffer-live-p parent-data) (kill-buffer parent-data))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Held diagnostics leave one inert review and one terminal result"
  (dolist (action '(reject remote-feedback view-kill))
    (mevedel-view-test--with-buffers
      (let* ((root (file-name-as-directory
                    (make-temp-file "mevedel-patch-settlement-" t)))
             (path (file-name-concat root "one.txt"))
             (workspace (mevedel-workspace--create
                         :type 'file :id root :root root :name "settlement"
                         :file-cache (mevedel-test-file-cache-create)))
             (session (mevedel-session-create "settlement" workspace root))
             (patch (string-join
                     '("*** Begin Patch"
                       "*** Update File: one.txt"
                       "@@"
                       "-old"
                       "+new"
                       "*** End Patch")
                     "\n"))
             results
             continuation
             proposal
             remote-feedback)
        (unwind-protect
            (progn
              (with-temp-file path (insert "old\n"))
              (with-current-buffer data-buf
                (setq-local default-directory root
                            mevedel--workspace workspace
                            mevedel--session session)
                (mevedel-tool-patch-handler
                 (lambda (value) (push value results))
                 (list :patch patch)))
              (with-current-buffer view-buf
                (goto-char (point-min))
                (search-forward "ApplyPatch ·")
                (setq proposal
                      (get-text-property (match-beginning 0)
                                         'mevedel-patch-proposal))
                (let* ((overlay (plist-get proposal :overlay))
                       (remote (overlay-get overlay 'mevedel--remote)))
                  (setq remote-feedback (plist-get remote :feedback)))
                (cl-letf (((symbol-function
                            'mevedel-edit-diagnostics-after-edit)
                           (lambda (_buffer _path callback)
                             (setq continuation callback))))
                  (mevedel-patch-review-submit))
                (should continuation)
                (should-not results)
                (let ((text (buffer-substring-no-properties
                             (point-min) mevedel-view--input-marker))
                      (remote (overlay-get (plist-get proposal :overlay)
                                           'mevedel--remote)))
                  (should (string-search "Applying patch" text))
                  (should-not (string-search "[ Reject all ]" text))
                  (should-not (plist-get remote :options))
                  (should-not (plist-get remote :feedback))))
              (pcase action
                ('reject
                 (with-current-buffer view-buf
                   (goto-char (point-min))
                   (search-forward "Applying patch")
                   (should-error (mevedel-patch-review-reject)
                                 :type 'user-error)))
                ('remote-feedback
                 (funcall remote-feedback "try something else")
                 (should-not (plist-get proposal :feedback)))
                ('view-kill
                 (kill-buffer view-buf)
                 (should (= 1 (length results)))
                 (should (equal "Error: Patch review aborted"
                                (plist-get (car results) :result)))))
              (funcall continuation)
              (should (= 1 (length results)))
              (unless (eq action 'view-kill)
                (should-not (plist-get (car results) :status)))
              (should (equal "new\n"
                             (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string)))))
          (when (file-directory-p root) (delete-directory root t))))))

  :doc "Sanitizes local paths and warns when review rollback is incomplete"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-review-rollback-" t)))
           (workspace (mevedel-workspace--create
                       :type 'test :id root :root root :name "rollback"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session--create
                     :name "rollback" :workspace workspace
                     :working-directory root :permission-mode 'ask))
           (first-address "local://first/one.txt")
           (second-address "local://second/two.txt")
           (patch (string-join
                   (list "*** Begin Patch"
                         (concat "*** Update File: " first-address)
                         "@@" "-old one" "+new one"
                         (concat "*** Update File: " second-address)
                         "@@" "-old two" "+new two"
                         "*** End Patch")
                   "\n"))
           first-directory
           first-path
           second-path
           first-buffer
           second-buffer
           result)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-session-persistence-shallow-ensure-files
               session data-buf)
              (let ((local-root
                     (file-name-concat (mevedel-session-save-path session)
                                       "local")))
                (setq first-path
                      (file-name-concat local-root "first" "one.txt")
                      second-path
                      (file-name-concat local-root "second" "two.txt")
                      first-directory (file-name-directory first-path)))
              (make-directory (file-name-directory first-path) t)
              (make-directory (file-name-directory second-path) t)
              (with-temp-file first-path (insert "old one\n"))
              (with-temp-file second-path (insert "old two\n"))
              (setq first-buffer (find-file-noselect first-path)
                    second-buffer (find-file-noselect second-path))
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer second-buffer
              (add-hook 'before-change-functions
                        (lambda (&rest _)
                          (set-file-modes first-directory #o500)
                          (error "Sync failure"))
                        nil t))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (mevedel-patch-review-submit)
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should-not result)
                (should (string-search first-address text))
                (should-not (string-search first-path text))
                (should (string-search "Sync failure" text))
                (should (string-search "Rollback was incomplete" text))
                (should-not (string-search "Deselect the stale file" text)))))
        (when (file-directory-p first-directory)
          (set-file-modes first-directory #o700))
        (dolist (buffer (list first-buffer second-buffer))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-reject
  (:doc "Reject-all clears every change and settles with an error result")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-reject-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "reject"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "reject" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+new"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "[ Reject all ]")
              (backward-char 2)
              (let ((command (key-binding (kbd "RET"))))
                (should (eq command #'mevedel-patch-review-reject))
                (call-interactively command)))
            (should (equal "Error: Patch rejected"
                           (plist-get result :result)))
            (should (eq 'error (plist-get result :status)))
            (should (equal "old\n"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-visit
  (:doc "RET visits the affected file at the hunk's baseline location")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-visit-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "visit"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "visit" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@ two"
                     "-old"
                     "+new"
                     "*** End Patch")
                   "\n"))
           visited result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "one\ntwo\nold\nfour\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ two")
              (should (eq (key-binding (kbd "RET"))
                          #'mevedel-patch-review-visit))
              (mevedel-patch-review-visit)
              (setq visited (current-buffer))
              (should (equal path (buffer-file-name visited)))
              (should (equal 3 (line-number-at-pos)))))
        (when (buffer-live-p visited) (kill-buffer visited))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review-local-visit
  (:doc "Review displays authored local addresses and visits resolved targets")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-local-visit-" t)))
           (workspace (mevedel-workspace--create
                       :type 'test :id root :root root :name "local-visit"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session--create
                     :name "local-visit" :workspace workspace
                     :working-directory root :permission-mode 'ask))
           (address "local://notes/one.txt")
           (patch (string-join
                   (list "*** Begin Patch"
                         (concat "*** Update File: " address)
                         "@@ two"
                         "-old"
                         "+new"
                         "*** End Patch")
                   "\n"))
           result visited local-path)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-session-persistence-shallow-ensure-files
               session data-buf)
              (setq local-path
                    (file-name-concat (mevedel-session-save-path session)
                                      "local" "notes" "one.txt"))
              (make-directory (file-name-directory local-path) t)
              (with-temp-file local-path
                (insert "one\ntwo\nold\nfour\n"))
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (should-not result)
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) mevedel-view--input-marker)))
                (should (string-search address text))
                (should-not (string-search local-path text)))
              (goto-char (point-min))
              (search-forward address)
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ two")
              (mevedel-patch-review-visit)
              (setq visited (current-buffer)))
            (should (equal local-path (buffer-file-name visited)))
            (should (= 3 (with-current-buffer visited
                           (line-number-at-pos))))
        (when (buffer-live-p visited) (kill-buffer visited))
        (when (file-directory-p root) (delete-directory root t)))))))

(mevedel-deftest mevedel-patch-review-next-row
  (:doc "n and p move between file and hunk rows")
  ,test
  (test)
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-patch-rows-" t)))
           (one (file-name-concat root "one.txt"))
           (workspace (mevedel-workspace--create
                       :type 'file :id root :root root :name "rows"
                       :file-cache (mevedel-test-file-cache-create)))
           (session (mevedel-session-create "rows" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+new"
                     "*** Add File: new.txt"
                     "+content"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file one (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          mevedel--workspace workspace
                          mevedel--session session)
              (mevedel-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (mevedel-patch-review-toggle-fold)
              (goto-char (point-min))
              (mevedel-patch-review-next-row)
              (should (looking-at-p "▼ ✓ M one\\.txt"))
              (mevedel-patch-review-next-row)
              (should (looking-at-p "    ✓ @@"))
              (mevedel-patch-review-next-row)
              (should (looking-at-p "▶ ✓ A new\\.txt"))
              (mevedel-patch-review-previous-row)
              (should (looking-at-p "    ✓ @@"))))
        (when (file-directory-p root) (delete-directory root t))))))

(mevedel-deftest mevedel-patch-review--feedback-targets
  (:doc "Lists the patch, every operation, and every hunk")
  ,test (test)
  (let* ((hunk '(:selected t))
         (update (list :kind 'update :hunks (list hunk)))
         (delete '(:kind delete))
         (proposal (list :operations (list update delete))))
    (should (equal (list proposal update hunk delete)
                   (mevedel-patch-review--feedback-targets proposal)))))

(mevedel-deftest mevedel-patch-review--target-at-point
  (:doc "Reads interaction metadata at point") ,test (test)
  (with-temp-buffer
    (insert (propertize "x" 'patch-target 'value))
    (goto-char (point-min))
    (should (eq 'value (mevedel-patch-review--target-at-point
                       'patch-target)))))

(mevedel-deftest mevedel-patch-review--insert-propertized
  (:doc "Makes review text read-only by default") ,test (test)
  (with-temp-buffer
    (mevedel-patch-review--insert-propertized "x")
    (should (get-text-property (point-min) 'read-only))))

(mevedel-deftest mevedel-patch-review--primary-label
  (:doc "Chooses the action label from staged state") ,test (test)
  (should (equal "Reject patch"
                 (mevedel-patch-review--primary-label
                  '(:operations ((:kind add :selected nil :content "x\n"))))))
  :doc "Counts selected changes, files, and staged comments"
  (should (equal "Apply 1 change in 1 file · send 1 comment"
                 (mevedel-patch-review--primary-label
                  '(:operations
                    ((:kind update
                      :hunks ((:selected t :diff-lines ("-x" "+y"))
                              (:selected nil :feedback "no"
                               :diff-lines ("-a" "+b")))))))))
  :doc "Requests revision when only comments are staged"
  (should (equal "Request revision · 1 comment"
                 (mevedel-patch-review--primary-label
                  '(:operations ((:kind add :selected nil :feedback "why"
                                  :content "x\n")))))))

(mevedel-deftest mevedel-patch-review--feedback-body
  (:doc "Renders staged feedback text") ,test (test)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (mevedel-patch-review--feedback-body nil nil '(:feedback "note") "file")
      (should (string-search "note" (buffer-string))))))

(mevedel-deftest mevedel-patch-review--body
  (:doc "Builds the aggregate review summary") ,test (test)
  (should (string-search
           "ApplyPatch · 1 file"
           (mevedel-patch-review--body
            '(:operations ((:kind add :rel-path "a" :content "x\n"
                            :selected t))))))

  :doc "Styles diff lines and shows the review controls"
  (let* ((body
          (mevedel-patch-review--body
           '(:operations
             ((:kind update :rel-path "a.el"
               :expanded t
               :hunks ((:selected t :diff-lines ("-old" "+new"))))))))
         (removed (string-search "│ old" body))
         (added (string-search "│ new" body)))
    (should (member 'diff-removed
                    (get-text-property (+ removed 2) 'font-lock-face body)))
    (should (member 'mevedel-patch-review-removed
                    (get-text-property (+ removed 2) 'font-lock-face body)))
    (should (member 'diff-added
                    (get-text-property (+ added 2) 'font-lock-face body)))
    (should (string-search "Keys:" body))
    (should (string-search "edit" body))
    (should (string-search "feedback" body)))

  :doc "Dims a deselected hunk instead of tinting it"
  (let* ((body
          (mevedel-patch-review--body
           '(:operations
             ((:kind update :rel-path "a.el"
               :expanded t
               :hunks ((:selected nil :diff-lines ("-old" "+new"))))))))
         (removed (string-search "│ old" body)))
    (should (equal '(shadow)
                   (get-text-property (+ removed 2) 'font-lock-face body)))
    (should (string-search "✗ @@" body)))

  :doc "Warns when review can race an active parent request"
  (let ((data-buffer (generate-new-buffer " *mevedel-side-review*")))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-side-conversation-parent-active-p)
                   (lambda (&optional buffer) (eq buffer data-buffer))))
          (should
           (string-search
            "parent request is still active"
            (mevedel-patch-review--body
             `(:data-buffer ,data-buffer
               :operations
               ((:kind add :rel-path "a" :content "x\n"
                 :selected t)))))))
      (kill-buffer data-buffer))))

(mevedel-deftest mevedel-patch-review--render
  (:doc "Registers one preview interaction") ,test (test)
  (mevedel-view-test--with-buffers
    (let ((proposal '(:id patch :operations nil)))
      (plist-put proposal :view-buffer view-buf)
      (with-current-buffer view-buf
        (should (overlayp (mevedel-patch-review--render proposal)))))))

(mevedel-deftest mevedel-patch-review-toggle-fold
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'mevedel-patch-review-toggle-fold)))

(mevedel-deftest mevedel-patch-review--feedback-input
  (:doc "Reads text between live feedback markers") ,test (test)
  (with-temp-buffer
    (let ((target (list :feedback-start (copy-marker (point-min)))))
      (insert "note")
      (plist-put target :feedback-end (copy-marker (point-max)))
      (add-text-properties (point-min) (point-max)
                           (list 'mevedel-patch-feedback-input target))
      (goto-char (point-min))
      (should (equal "note" (cdr (mevedel-patch-review--feedback-input)))))))

(mevedel-deftest mevedel-patch-review--clear-feedback-markers
  (:doc "Releases feedback markers") ,test (test)
  (with-temp-buffer
    (let ((target (list :feedback-start (copy-marker (point-min))
                        :feedback-end (copy-marker (point-min)))))
      (mevedel-patch-review--clear-feedback-markers target)
      (should-not (plist-get target :feedback-start)))))

(mevedel-deftest mevedel-patch-review-confirm-feedback
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'mevedel-patch-review-confirm-feedback)))

(mevedel-deftest mevedel-patch-review-cancel-feedback
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'mevedel-patch-review-cancel-feedback)))

(mevedel-deftest mevedel-patch-review-edit-commit
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'mevedel-patch-review-edit-commit)))

(mevedel-deftest mevedel-patch-review-edit-cancel
  (:doc "Kills the active hunk edit buffer") ,test (test)
  (let ((buffer (generate-new-buffer " *patch-edit-cancel*")))
    (with-current-buffer buffer (mevedel-patch-review-edit-cancel))
    (should-not (buffer-live-p buffer))))

(provide 'test-mevedel-patch-review)
;;; test-mevedel-patch-review.el ends here
