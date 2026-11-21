;;; test-niwa-patch-review.el --- Tests for the ApplyPatch review UI -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for the staged ApplyPatch review interface.

;;; Code:

(require 'niwa)
(require 'niwa-tool-patch)
(require 'niwa-patch-review)
(require 'niwa-view)
(require 'niwa-view-interaction)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
            "helpers"))
(require 'niwa-session-persistence)
(require 'ediff)

(defvar niwa-session--read-only-mode nil)

(defun niwa-patch-review-test--edit-quit (view-buffer adopt)
  "Quit VIEW-BUFFER's side-by-side edit, answering ADOPT to its prompt.
ediff's own quit confirmation is always accepted; only the adopt prompt
follows ADOPT, so a declined revision still closes the session."
  (with-current-buffer
      (buffer-local-value 'niwa-patch-review--edit-session view-buffer)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt &rest _)
                 (if (string-prefix-p "Adopt" prompt) adopt t))))
      (ediff-quit nil))))

(defun niwa-patch-review-test--edit (view-buffer search text &optional keep)
  "Edit the change at SEARCH in VIEW-BUFFER, replacing its result with TEXT.
TEXT nil leaves the editable buffer untouched.  KEEP non-nil declines the
adopt prompt.  Returns the messages the session produced."
  (let (captured)
    (niwa-test--with-captured-diagnostics captured
      (let ((editable (with-current-buffer view-buffer
                        (goto-char (point-min))
                        (search-forward search)
                        (niwa-patch-review-edit))))
        (when text
          (with-current-buffer editable
            (erase-buffer)
            (insert text)))
        (niwa-patch-review-test--edit-quit view-buffer (not keep))))
    captured))

(niwa-deftest niwa-patch-review-start
  (:doc "One hierarchical review stages hunk selection before aggregate apply")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-review-" t)))
           (one (file-name-concat root "one.txt"))
           (two (file-name-concat root "two.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "review"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "review" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (should-not result)
            (should (equal "a\nold1\nmiddle\nold2\nz\n"
                           (niwa-test--read-file one)))
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
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
              (niwa-patch-review-toggle-fold)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "- │ old1" text))
                (should (string-search "@@ a" text))
                (should-not (string-search "│ before" text)))
              (goto-char (point-min))
              (search-forward "@@ a")
              (niwa-patch-review-toggle-fold)
              (should-not (string-search
                           "- │ old1"
                           (buffer-substring-no-properties
                            (point-min) (niwa-view-transcript-tail))))
              (goto-char (point-min))
              (search-forward "@@ a")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ a")
              (niwa-patch-review-toggle-selection)
              (should (string-search
                       "ApplyPatch · 2 files · 2/3 changes · +2 −2"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "a\nold1\nmiddle\nnew2\nz\n"
                           (niwa-test--read-file one)))
            (should (equal "after\n"
                           (niwa-test--read-file two)))
            (should (string-search "Rejected: one.txt hunk 1"
                                   (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t))))))

(niwa-deftest niwa-patch-review-feedback
  (:doc "Per-hunk multiline feedback stages independent revision notes")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-feedback-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "feedback"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "feedback" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (should (get-text-property (1- (point)) 'read-only))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ first")
              (setq proposal (get-text-property
                              (1- (point)) 'niwa-patch-proposal))
              (niwa-patch-review-feedback)
              (should-not (get-text-property (point) 'read-only))
              (should (string-search
                       "C-c C-c stage feedback"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              (insert "unfinished draft")
              (niwa-patch-review--render proposal)
              (should (string-search
                       "unfinished draft"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              ;; A foreign zone re-render and a queue rebuild (the
              ;; control-transfer poll path) draw the live draft through
              ;; the descriptor body function instead of a registration
              ;; snapshot, and typing continues where it left off.
              (goto-char (point-min))
              (search-forward "unfinished draft")
              (niwa-view--interaction-render)
              (should (string-search
                       "unfinished draft"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              (niwa-view--interaction-rebuild)
              (should (string-search
                       "unfinished draft"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              (goto-char (point-min))
              (search-forward "unfinished draft")
              (dolist (c (string-to-list " extended"))
                (insert-and-inherit c))
              (niwa-view--interaction-rebuild)
              (should (string-search
                       "unfinished draft extended"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              ;; Folding the file must keep the live feedback editor
              ;; rendered so its draft and markers survive redraws.
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (should (string-search
                       "unfinished draft"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              (should (text-property-any
                       (point-min) (point-max)
                       'niwa-patch-feedback-input
                       (car (plist-get
                             (car (plist-get proposal :operations))
                             :hunks))))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (text-property-any
                          (point-min) (point-max)
                          'niwa-patch-feedback-input
                          (car (plist-get
                                (car (plist-get proposal :operations))
                                :hunks))))
              (niwa-patch-review-cancel-feedback)
              (goto-char (point-min))
              (search-forward "✓ @@ first")
              (niwa-patch-review-feedback)
              (insert "Keep the old public behavior.\nIt is documented.")
              (niwa-patch-review-confirm-feedback)
              (goto-char (point-min))
              (search-forward "@@ second")
              (niwa-patch-review-feedback)
              (insert "Use the newer API instead.")
              (niwa-patch-review-confirm-feedback)
              ;; File-level feedback on an Update file keeps hunk
              ;; selection untouched.
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-feedback)
              (insert "Keep this file minimal.")
              (niwa-patch-review-confirm-feedback)
              ;; Patch-level feedback from the header.
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-feedback)
              (insert "Overall: split this into two patches.")
              (niwa-patch-review-confirm-feedback)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "[ Request revision · 4 comments ]" text))
                (should (string-search "Keep the old public behavior." text))
                (should (string-search "Use the newer API instead." text))
                (should (string-search "Keep this file minimal." text))
                (should (string-search "Overall: split this into two patches."
                                       text)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "first\nold1\nsecond\nold2\n"
                           (niwa-test--read-file path)))
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

(niwa-deftest niwa-patch-review-toggle-selection
  (:doc "Add, Delete, and Move remain whole-operation review choices")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-whole-" t)))
           (delete-path (file-name-concat root "delete.txt"))
           (move-path (file-name-concat root "move.txt"))
           (moved-path (file-name-concat root "moved.txt"))
           (added-path (file-name-concat root "new.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "whole"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "whole" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "D delete.txt")
              (niwa-patch-review-toggle-selection)
              (goto-char (point-min))
              (search-forward "R move.txt → moved.txt")
              (niwa-patch-review-toggle-fold)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "- │ old" text))
                ;; Move diffs stay whole-operation: no selectable hunk rows.
                (should-not (string-search "@@" text))
                (should-not
                 (or (get-text-property (point-min) 'niwa-patch-hunk)
                     (next-single-property-change (point-min)
                                                  'niwa-patch-hunk))))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (file-exists-p delete-path))
            (should (equal "added\n"
                           (niwa-test--read-file added-path)))
            (should-not (file-exists-p move-path))
            (should (equal "moved\n"
                           (niwa-test--read-file moved-path)))
            (should (string-search "Rejected: delete.txt"
                                   (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "SPC on an Update file row toggles every hunk in the file"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-filewide-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "filewide"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "filewide" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-selection)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "0/2 changes" text))
                (should (string-search "✗ @@ first" text))
                (should (string-search "✗ @@ second" text))
                (should (string-search "[ Reject patch ]" text)))
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-selection)
              (should (string-search
                       "2/2 changes"
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail))))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "first\nnew1\nsecond\nnew2\n"
                           (niwa-test--read-file path))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "a hidden locator does not make file-row reselection take two presses"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-locator-select-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "locator-select"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create
                     "locator-select" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     " section"
                     " same"
                     "@@"
                     "-same"
                     "+changed"
                     "*** End Patch")
                   "\n")))
      (unwind-protect
          (progn
            (with-temp-file path (insert "section\nsame\nsame\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler #'ignore (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (goto-char
               (next-single-property-change
                (point) 'niwa-patch-hunk nil (niwa-view-transcript-tail)))
              (niwa-patch-review-toggle-selection)
              (goto-char (point-min))
              (search-forward "M one.txt")
              (let* ((operation
                      (niwa-patch-review--target-at-point
                       'niwa-patch-operation))
                     (change
                      (seq-find #'niwa-tool-patch-hunk-changes-p
                                (plist-get operation :hunks))))
                (should-not (plist-get change :selected))
                (niwa-patch-review-toggle-selection)
                (should (plist-get change :selected)))))
        (delete-directory root t)))))

(niwa-deftest niwa-patch-review-edit
  (:doc "Editing an Update re-derives its hunks from the edited result")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "edit"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "edit" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+model"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold))
            (niwa-patch-review-test--edit
             view-buf "@@ old" "user edited\n")
            (with-current-buffer view-buf
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "+ │ user edited" text))
                (should (string-search "· edited" text)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "user edited\n"
                           (niwa-test--read-file path)))
            (should (string-search
                     (concat "User edited during review (authoritative):"
                             " one.txt (whole file revised)")
                     (plist-get result :result)))
            (should (string-search "do not revert" (plist-get result :result)))
            (should (string-search "(1 revised by the user during review)"
                                   (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Quitting an untouched session stages nothing"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-noop-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "noop"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "noop" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+model"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (niwa-patch-review-test--edit view-buf "M one.txt" nil)
            (should-not niwa-patch-review--edit-session)
            (with-current-buffer view-buf
              (should-not
               (string-search "· edited"
                              (buffer-substring-no-properties
                               (point-min) (niwa-view-transcript-tail))))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "model\n"
                           (niwa-test--read-file path)))
            (should-not (string-search "revised by the user"
                                       (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Declining the adopt prompt discards the edits"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-decline-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "decline"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "decline" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+model"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (niwa-patch-review-test--edit
             view-buf "M one.txt" "discarded\n" t)
            (should-not niwa-patch-review--edit-session)
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "model\n"
                           (niwa-test--read-file path)))
            (should-not (string-search "revised by the user"
                                       (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A deselected hunk survives an edit and stays reselectable"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-keep-" t)))
           (path (file-name-concat root "two.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "keep"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "keep" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: two.txt"
                     "@@"
                     " head"
                     "-first"
                     "+FIRST"
                     "@@"
                     " tail"
                     "-second"
                     "+SECOND"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "head\nfirst\nmiddle\nmiddle\nmiddle\n"
                      "tail\nsecond\nend\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            ;; Deselect the second hunk, then rewrite the file around it.
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M two.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ tail")
              (niwa-patch-review-toggle-selection))
            (niwa-patch-review-test--edit
             view-buf "@@ head"
             (concat "head\nEDITED\nmiddle\nmiddle\nmiddle\n"
                     "tail\nsecond\nend\n"))
            (let* ((proposal (with-current-buffer view-buf
                               (goto-char (point-min))
                               (search-forward "M two.txt")
                               (get-text-property (point)
                                                  'niwa-patch-proposal)))
                   (hunks (plist-get (car (plist-get proposal :operations))
                                     :hunks)))
              ;; The rejected hunk is still there, still deselected, and
              ;; still in file order behind the derived one.
              (should (= 2 (length hunks)))
              (should (plist-get (nth 0 hunks) :selected))
              (should (plist-get (nth 0 hunks) :modified))
              (should-not (plist-get (nth 1 hunks) :selected))
              (should-not (plist-get (nth 1 hunks) :modified))
              (should (member "+SECOND" (plist-get (nth 1 hunks) :diff-lines)))
              ;; Reselecting it applies on top of the edited result.
              (plist-put (nth 1 hunks) :selected t))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal (concat "head\nEDITED\nmiddle\nmiddle\nmiddle\n"
                                   "tail\nSECOND\nend\n")
                           (niwa-test--read-file path)))
            (should (string-search "two.txt (whole file revised)"
                                   (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Feedback on a deselected hunk survives an edit of its file"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-fb-" t)))
           (path (file-name-concat root "two.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "edit-fb"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "edit-fb" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: two.txt"
                     "@@"
                     " head"
                     "-first"
                     "+FIRST"
                     "@@"
                     " tail"
                     "-second"
                     "+SECOND"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "head\nfirst\nmiddle\nmiddle\nmiddle\n"
                      "tail\nsecond\nend\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M two.txt")
              (niwa-patch-review-toggle-fold)
              ;; Hunk feedback deselects its hunk, so that hunk is the
              ;; one an edit carries across untouched.
              (goto-char (point-min))
              (search-forward "@@ tail")
              (niwa-patch-review-feedback)
              (insert "Leave the second one alone.")
              (niwa-patch-review-confirm-feedback))
            (niwa-patch-review-test--edit
             view-buf "@@ head"
             (concat "head\nEDITED\nmiddle\nmiddle\nmiddle\n"
                     "tail\nsecond\nend\n"))
            (let* ((proposal (with-current-buffer view-buf
                               (goto-char (point-min))
                               (search-forward "M two.txt")
                               (get-text-property (point)
                                                  'niwa-patch-proposal)))
                   (hunks (plist-get (car (plist-get proposal :operations))
                                     :hunks)))
              (should (= 2 (length hunks)))
              (should-not (plist-get (nth 1 hunks) :selected))
              (should (equal "Leave the second one alone."
                             (plist-get (nth 1 hunks) :feedback)))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Editing an Add rewrites its content and queues the revision reminder"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-add-" t)))
           (path (file-name-concat root "fresh.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "edit-add"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "edit-add" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: fresh.txt"
                     "+model content"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (niwa-patch-review-test--edit
             view-buf "A fresh.txt" "user content\n")
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "A fresh.txt")
              (niwa-patch-review-toggle-fold)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "+ │ user content" text))
                (should (string-search "· edited" text)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "user content\n"
                           (niwa-test--read-file path)))
            (should (string-search
                     "User edited during review (authoritative): fresh.txt"
                     (plist-get result :result)))
            (should (string-search "(1 revised by the user during review)"
                                   (plist-get result :result)))
            (let ((reminders (niwa-session-reminders session)))
              (should (memq 'user-revised-patch
                            (mapcar #'niwa-reminder-type reminders)))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Adopting an Add clears the feedback that deselected it"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-fbclear-" t)))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "fb-clear"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "fb-clear" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: fresh.txt"
                     "+model content"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "A fresh.txt")
              (niwa-patch-review-feedback)
              (insert "Put this in the other file.")
              (niwa-patch-review-confirm-feedback))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (niwa-patch-review-test--edit
               view-buf "A fresh.txt" "other content\n"))
            (let* ((proposal (with-current-buffer view-buf
                               (goto-char (point-min))
                               (search-forward "A fresh.txt")
                               (get-text-property (point)
                                                  'niwa-patch-proposal)))
                   (operation (car (plist-get proposal :operations))))
              (should-not (plist-get operation :feedback))
              (should (plist-get operation :selected))
              (should (equal "other content\n"
                             (plist-get operation :content)))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "An emptied Add is refused and leaves its content staged"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-empty-" t)))
           (path (file-name-concat root "fresh.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "empty-add"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "empty-add" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: fresh.txt"
                     "+model content"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (should (string-search
                     "An added file needs content"
                     (niwa-patch-review-test--edit
                      view-buf "A fresh.txt" "")))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "model content\n"
                           (niwa-test--read-file path)))
            (should-not (string-search "revised by the user"
                                       (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A revision that cannot be staged is reported and rolled back"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-stale-" t)))
           (path (file-name-concat root "fresh.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "stale-add"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "stale-add" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: fresh.txt"
                     "+model content"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            ;; The file appears underneath the review, so validating the
            ;; revision fails and the staged content must survive intact.
            (with-temp-file path (insert "someone else\n"))
            (should (string-search
                     "Cannot add existing file"
                     (niwa-patch-review-test--edit
                      view-buf "A fresh.txt" "user content\n")))
            (let* ((proposal (with-current-buffer view-buf
                               (goto-char (point-min))
                               (search-forward "A fresh.txt")
                               (get-text-property (point)
                                                  'niwa-patch-proposal)))
                   (operation (car (plist-get proposal :operations))))
              (should (equal "model content\n"
                             (plist-get operation :content)))
              (should-not (plist-get operation :modified))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Editing a Delete keeps the file with the content the user writes"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-del-" t)))
           (path (file-name-concat root "gone.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "edit-del"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "edit-del" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Delete File: gone.txt"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "keep me\nobsolete\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            ;; An emptied right-hand side keeps the Delete as proposed.
            (should (string-search
                     "Empty content keeps nothing"
                     (niwa-patch-review-test--edit
                      view-buf "D gone.txt" "\n")))
            (niwa-patch-review-test--edit
             view-buf "D gone.txt" "keep me\n")
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M gone.txt")
              (niwa-patch-review-toggle-fold)
              (let ((text (buffer-substring-no-properties
                           (point-min) (niwa-view-transcript-tail))))
                (should (string-search "M gone.txt" text))
                (should (string-search "- │ obsolete" text))
                (should (string-search "· edited" text)))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should (equal "keep me\n"
                           (niwa-test--read-file path)))
            (should (string-search
                     (concat "User edited during review (authoritative):"
                             " gone.txt (whole file revised)")
                     (plist-get result :result)))
            (should (memq 'user-revised-patch
                          (mapcar #'niwa-reminder-type
                                  (niwa-session-reminders session)))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Keeping a Delete's whole content is refused as no change at all"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-whole-" t)))
           (path (file-name-concat root "gone.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "whole-del"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "whole-del" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Delete File: gone.txt"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "every line\nstays\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (should (string-search
                     "keeps the file unchanged"
                     (niwa-patch-review-test--edit
                      view-buf "D gone.txt" "every line\nstays\n")))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (should (search-forward "D gone.txt" nil t)))
            (ignore result))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A deselected indivisible operation has nothing to compare"
  (niwa-view-test--with-buffers
    (let ((proposal (list :view-buffer view-buf)))
      (dolist (operation
               '((:kind add :rel-path "new.txt" :content "new\n"
                  :selected nil)
                 (:kind delete :rel-path "old.txt"
                  :baseline-content "old\n" :selected nil)
                 (:kind move :rel-path "from.txt" :move-rel-path "to.txt"
                  :path "/tmp/from.txt" :baseline-content "old\n"
                  :selected nil
                  :hunks ((:selected t :old-lines ("old")
                           :new-lines ("new")
                           :diff-lines ("-old" "+new"))))))
        (with-current-buffer view-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize
                     "change"
                     'niwa-patch-proposal proposal
                     'niwa-patch-operation operation)))
          (goto-char (point-min))
          (should-error (niwa-patch-review-edit) :type 'user-error)))))

  :doc "Refuses a second session, an identical result, and a pure rename"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-refuse-" t)))
           (path (file-name-concat root "one.txt"))
           (renamed (file-name-concat root "old.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "refuse"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "refuse" workspace root))
           (other-view (generate-new-buffer " *other-patch-view*"))
           (other-control (generate-new-buffer " *other-ediff-control*"))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+model"
                     "*** Update File: old.txt"
                     "*** Move to: new.txt"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-temp-file renamed (insert "unchanged\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            ;; A live editor owned by another session view does not block this
            ;; review; the same view is still limited to one editor below.
            (with-current-buffer other-view
              (setq-local niwa-patch-review--edit-session other-control))
            (with-current-buffer view-buf
              ;; A pure rename has no content to compare.
              (goto-char (point-min))
              (search-forward "old.txt")
              (should-error (niwa-patch-review-edit) :type 'user-error)
              ;; Deselecting every hunk leaves the two sides equal.
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-selection)
              (goto-char (point-min))
              (search-forward "M one.txt")
              (should-error (niwa-patch-review-edit) :type 'user-error)
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-selection))
            (niwa-test--with-captured-diagnostics nil
              (let ((editable (with-current-buffer view-buf
                                (goto-char (point-min))
                                (search-forward "M one.txt")
                                (niwa-patch-review-edit))))
                (should (buffer-live-p editable))
                (with-current-buffer view-buf
                  (goto-char (point-min))
                  (search-forward "M one.txt")
                  (should-error (niwa-patch-review-edit)
                                :type 'user-error))
                (niwa-patch-review-test--edit-quit view-buf nil)
                (should-not (buffer-live-p editable))))
            (should-not
             (buffer-local-value
              'niwa-patch-review--edit-session view-buf))
            (ignore result))
        (when (buffer-live-p other-view) (kill-buffer other-view))
        (when (buffer-live-p other-control) (kill-buffer other-control))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A settled review discards a revision instead of staging it"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-settled-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "settled"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "settled" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: one.txt"
                     "@@"
                     "-old"
                     "+model"
                     "*** End Patch")
                   "\n"))
           result captured)
      (unwind-protect
          (progn
            (with-temp-file path (insert "old\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (niwa-test--with-captured-diagnostics captured
              (let ((editable (with-current-buffer view-buf
                                (goto-char (point-min))
                                (search-forward "M one.txt")
                                (niwa-patch-review-edit))))
                (with-current-buffer view-buf
                  (goto-char (point-min))
                  (search-forward "ApplyPatch ·")
                  (niwa-patch-review-submit))
                (with-current-buffer editable
                  (erase-buffer)
                  (insert "too late\n"))
                (niwa-patch-review-test--edit-quit view-buf t)))
            (should (string-search "already settled" captured))
            (should (equal "model\n"
                           (niwa-test--read-file path)))
            (should-not (string-search "revised by the user"
                                       (plist-get result :result))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A selection changed while the editor was open discards the revision"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-race-" t)))
           (path (file-name-concat root "two.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "race"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "race" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: two.txt"
                     "@@"
                     " head"
                     "-first"
                     "+FIRST"
                     "@@"
                     " tail"
                     "-second"
                     "+SECOND"
                     "*** End Patch")
                   "\n"))
           result captured)
      (unwind-protect
          (progn
            (with-temp-file path
              (insert "head\nfirst\nmiddle\nmiddle\nmiddle\n"
                      "tail\nsecond\nend\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M two.txt")
              (niwa-patch-review-toggle-fold))
            (niwa-test--with-captured-diagnostics captured
              (let ((editable (with-current-buffer view-buf
                                (goto-char (point-min))
                                (search-forward "@@ head")
                                (niwa-patch-review-edit))))
                ;; The review stays interactive while a session is live.
                (with-current-buffer view-buf
                  (goto-char (point-min))
                  (search-forward "@@ tail")
                  (niwa-patch-review-toggle-selection))
                (with-current-buffer editable
                  (erase-buffer)
                  (insert "head\nEDITED\nmiddle\nmiddle\nmiddle\n"
                          "tail\nSECOND\nend\n"))
                (niwa-patch-review-test--edit-quit view-buf t)))
            (should (string-search "selection changed" captured))
            (let* ((proposal (with-current-buffer view-buf
                               (goto-char (point-min))
                               (search-forward "M two.txt")
                               (get-text-property (point)
                                                  'niwa-patch-proposal)))
                   (hunks (plist-get (car (plist-get proposal :operations))
                                     :hunks)))
              ;; The rejected hunk stays rejected and nothing is revised.
              (should (= 2 (length hunks)))
              (should-not (plist-get (nth 1 hunks) :selected))
              (should-not (cl-some (lambda (hunk) (plist-get hunk :modified))
                                   hunks))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Declining the adoption keeps an Add's feedback"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-edit-fbkeep-" t)))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "fb-keep"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "fb-keep" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: fresh.txt"
                     "+model content"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "A fresh.txt")
              (niwa-patch-review-feedback)
              (insert "Put this in the other file.")
              (niwa-patch-review-confirm-feedback))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (niwa-patch-review-test--edit
               view-buf "A fresh.txt" "other content\n" t))
            (let* ((proposal (with-current-buffer view-buf
                               (goto-char (point-min))
                               (search-forward "A fresh.txt")
                               (get-text-property (point)
                                                  'niwa-patch-proposal)))
                   (operation (car (plist-get proposal :operations))))
              (should (equal "Put this in the other file."
                             (plist-get operation :feedback)))
              (should (equal "model content\n"
                             (plist-get operation :content)))
              (should-not (plist-get operation :selected))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "An unedited Delete still applies and queues no reminder"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-plain-del-" t)))
           (path (file-name-concat root "gone.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "plain-del"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "plain-del" workspace root))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Delete File: gone.txt"
                     "*** End Patch")
                   "\n"))
           result)
      (unwind-protect
          (progn
            (with-temp-file path (insert "obsolete\n"))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should-not (file-exists-p path))
            (should-not (string-search "revised by the user"
                                       (plist-get result :result)))
            (should-not (memq 'user-revised-patch
                              (mapcar #'niwa-reminder-type
                                      (niwa-session-reminders session)))))
        (when (file-directory-p root) (delete-directory root t))))))

(niwa-deftest niwa-patch-review-submit
  (:doc "A stale selected file writes nothing and leaves review recoverable")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-stale-" t)))
           (one (file-name-concat root "one.txt"))
           (two (file-name-concat root "two.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "stale"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "stale" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-temp-file two (insert "external change\n"))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit)
              (should-not result)
              (should (string-search
                       (format "niwa [%s]: File changed during review: two.txt (patch conflict)" root)
                       (buffer-substring-no-properties
                        (point-min) (niwa-view-transcript-tail)))))
            (should (equal "old one\n"
                           (niwa-test--read-file one)))
            (should (equal "external change\n"
                           (niwa-test--read-file two))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "RET on Apply selected writes the patch and settles the callback"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-submit-key-" t)))
           (path (file-name-concat root "test.el"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "submit-key"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "submit-key" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "[ Apply 1 change in 1 file ]")
              (backward-char 2)
              (let ((command (key-binding (kbd "RET"))))
                (should (eq command #'niwa-patch-review-submit))
                (call-interactively command)))
            (should result)
            (should (equal "(defun hello-world ()\n  (interactive)\n  (message \"Hello, world!\"))\n"
                           (niwa-test--read-file path))))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "A newly active parent is disclosed before the patch can apply"
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-parent-race-" t)))
           (path (file-name-concat root "one.txt"))
           (parent-data (generate-new-buffer " *niwa-patch-parent*"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "parent-race"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "parent-race" workspace root))
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
              (setq-local niwa--current-request nil))
            (with-current-buffer data-buf
              (setq-local default-directory root
                          niwa--workspace workspace
                          niwa--session session
                          niwa-side-conversation--parent-buffer parent-data)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (should-not
               (string-search
                "parent request is still active"
                (buffer-substring-no-properties
                 (point-min) (niwa-view-transcript-tail)))))
            (with-current-buffer parent-data
              (setq-local niwa--current-request
                  (niwa-request--create :session niwa--session)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit)
              (should-not result)
              (should
               (string-search
                "parent request is still active"
                (buffer-substring-no-properties
                 (point-min) (niwa-view-transcript-tail))))
              (goto-char (point-min))
              (search-forward "ApplyPatch ·")
              (niwa-patch-review-submit))
            (should result)
            (should (equal "new\n"
                           (niwa-test--read-file path))))
        (when (buffer-live-p parent-data) (kill-buffer parent-data))
        (when (file-directory-p root) (delete-directory root t)))))

  :doc "Held diagnostics leave one inert review and one terminal result"
  (dolist (action '(reject remote-feedback view-kill))
    (niwa-view-test--with-buffers
      (let* ((root (file-name-as-directory
                    (make-temp-file "niwa-patch-settlement-" t)))
             (path (file-name-concat root "one.txt"))
             (workspace (niwa-workspace--create
                         :type 'file :id root :root root :name "settlement"
                         :file-cache (niwa-test-file-cache-create)))
             (session (niwa-session-create "settlement" workspace root))
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
                            niwa--workspace workspace
                            niwa--session session)
                (niwa-tool-patch-handler
                 (lambda (value) (push value results))
                 (list :patch patch)))
              (with-current-buffer view-buf
                (goto-char (point-min))
                (search-forward "ApplyPatch ·")
                (setq proposal
                      (get-text-property (match-beginning 0)
                                         'niwa-patch-proposal))
                (let* ((overlay (plist-get proposal :overlay))
                       (remote (overlay-get overlay 'niwa--remote)))
                  (setq remote-feedback (plist-get remote :feedback)))
                (cl-letf (((symbol-function
                            'niwa-edit-diagnostics-after-edit)
                           (lambda (_buffer _path callback)
                             (setq continuation callback))))
                  (niwa-patch-review-submit))
                (should continuation)
                (should-not results)
                (let ((text (buffer-substring-no-properties
                             (point-min) (niwa-view-transcript-tail)))
                      (remote (overlay-get (plist-get proposal :overlay)
                                           'niwa--remote)))
                  (should (string-search "Applying patch" text))
                  (should-not (string-search "[ Reject all ]" text))
                  (should-not (plist-get remote :options))
                  (should-not (plist-get remote :feedback))))
              (pcase action
                ('reject
                 (with-current-buffer view-buf
                   (goto-char (point-min))
                   (search-forward "Applying patch")
                   (should-error (niwa-patch-review-reject)
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
                             (niwa-test--read-file path))))
          (when (file-directory-p root) (delete-directory root t))))))

  :doc "Sanitizes local paths and warns when review rollback is incomplete"
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-patch-review-rollback-" t))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let* ((workspace (niwa-workspace--create
                             :type 'file :id root :root root :name "rollback"
                             :file-cache (niwa-test-file-cache-create)))
                 (session (let ((session (niwa-session-create
                                          "rollback" workspace root "rollback-id")))
                            (setf (niwa-session-permission-mode session) 'ask)
                            session))
                 (first-address "work://first/one.txt")
                 (second-address "work://second/two.txt")
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
                                niwa--workspace workspace
                                niwa--session session)
                    (niwa-session-persistence-shallow-ensure-files
                     session data-buf)
                    (let ((local-root
                           (file-name-concat (niwa-session-save-path session)
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
                    (niwa-tool-patch-handler
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
                    (niwa-patch-review-submit)
                    (let ((text (buffer-substring-no-properties
                                 (point-min) (niwa-view-transcript-tail))))
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
              (when-let* ((save-path (niwa-session-save-path session)))
                (niwa-session-persistence-lock-release save-path session)))))
      ;; Killing the view buffers records telemetry under the session's
      ;; save path, so the directory goes only after them.
      (delete-directory root t))))

(niwa-deftest niwa-patch-review-reject
  (:doc "Reject-all clears every change and settles with an error result")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-reject-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "reject"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "reject" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "[ Reject all ]")
              (backward-char 2)
              (let ((command (key-binding (kbd "RET"))))
                (should (eq command #'niwa-patch-review-reject))
                (call-interactively command)))
            (should (equal "Error: Patch rejected"
                           (plist-get result :result)))
            (should (eq 'error (plist-get result :status)))
            (should (equal "old\n"
                           (niwa-test--read-file path))))
        (when (file-directory-p root) (delete-directory root t))))))

(niwa-deftest niwa-patch-review-visit
  (:doc "RET visits the affected file at the hunk's baseline location")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-visit-" t)))
           (path (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "visit"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "visit" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (search-forward "@@ two")
              (should (eq (key-binding (kbd "RET"))
                          #'niwa-patch-review-visit))
              (niwa-patch-review-visit)
              (setq visited (current-buffer))
              (should (equal path (buffer-file-name visited)))
              (should (equal 3 (line-number-at-pos)))))
        (when (buffer-live-p visited) (kill-buffer visited))
        (when (file-directory-p root) (delete-directory root t))))))

(niwa-deftest niwa-patch-review-local-visit
  (:doc "Review displays authored local addresses and visits resolved targets")
  ,test
  (test)
  (let ((root (file-name-as-directory
               (make-temp-file "niwa-patch-local-visit-" t))))
    (unwind-protect
        (niwa-view-test--with-buffers
          (let* ((workspace (niwa-workspace--create
                             :type 'file :id root :root root :name "local-visit"
                             :file-cache (niwa-test-file-cache-create)))
                 (session (let ((session (niwa-session-create
                                          "local-visit" workspace root "local-visit-id")))
                            (setf (niwa-session-permission-mode session) 'ask)
                            session))
                 (address "work://notes/one.txt")
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
                                niwa--workspace workspace
                                niwa--session session)
                    (niwa-session-persistence-shallow-ensure-files
                     session data-buf)
                    (setq local-path
                          (file-name-concat (niwa-session-save-path session)
                                            "local" "notes" "one.txt"))
                    (make-directory (file-name-directory local-path) t)
                    (with-temp-file local-path
                      (insert "one\ntwo\nold\nfour\n"))
                    (niwa-tool-patch-handler
                     (lambda (value) (setq result value))
                     (list :patch patch)))
                  (should-not result)
                  (with-current-buffer view-buf
                    (let ((text (buffer-substring-no-properties
                                 (point-min) (niwa-view-transcript-tail))))
                      (should (string-search address text))
                      (should-not (string-search local-path text)))
                    (goto-char (point-min))
                    (search-forward address)
                    (niwa-patch-review-toggle-fold)
                    (goto-char (point-min))
                    (search-forward "@@ two")
                    (niwa-patch-review-visit)
                    (setq visited (current-buffer)))
                  (should (equal local-path (buffer-file-name visited)))
                  (should (= 3 (with-current-buffer visited
                                 (line-number-at-pos)))))
              (when (buffer-live-p visited) (kill-buffer visited))
              (when-let* ((save-path (niwa-session-save-path session)))
                (niwa-session-persistence-lock-release save-path session)))))
      ;; Killing the view buffers records telemetry under the session's
      ;; save path, so the directory goes only after them.
      (delete-directory root t))))

(niwa-deftest niwa-patch-review-next-row
  (:doc "n and p move between file and hunk rows")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "niwa-patch-rows-" t)))
           (one (file-name-concat root "one.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "rows"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session-create "rows" workspace root))
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
                          niwa--workspace workspace
                          niwa--session session)
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch patch)))
            (with-current-buffer view-buf
              (goto-char (point-min))
              (search-forward "M one.txt")
              (niwa-patch-review-toggle-fold)
              (goto-char (point-min))
              (niwa-patch-review-next-row)
              (should (looking-at-p "▼ ✓ M one\\.txt"))
              (niwa-patch-review-next-row)
              (should (looking-at-p "    ✓ @@"))
              (niwa-patch-review-next-row)
              (should (looking-at-p "▶ ✓ A new\\.txt"))
              (niwa-patch-review-previous-row)
              (should (looking-at-p "    ✓ @@"))))
        (when (file-directory-p root) (delete-directory root t))))))

(niwa-deftest niwa-patch-review--feedback-targets
  (:doc "Lists the patch, every operation, and every hunk")
  ,test (test)
  (let* ((hunk '(:selected t))
         (update (list :kind 'update :hunks (list hunk)))
         (delete '(:kind delete))
         (proposal (list :operations (list update delete))))
    (should (equal (list proposal update hunk delete)
                   (niwa-patch-review--feedback-targets proposal)))))

(niwa-deftest niwa-patch-review--target-at-point
  (:doc "Reads interaction metadata at point") ,test (test)
  (with-temp-buffer
    (insert (propertize "x" 'patch-target 'value))
    (goto-char (point-min))
    (should (eq 'value (niwa-patch-review--target-at-point
                       'patch-target)))))

(niwa-deftest niwa-patch-review--insert-propertized
  (:doc "Makes review text read-only by default") ,test (test)
  (with-temp-buffer
    (niwa-patch-review--insert-propertized "x")
    (should (get-text-property (point-min) 'read-only))))

(niwa-deftest niwa-patch-review--primary-label
  (:doc "Chooses the action label from staged state") ,test (test)
  (should (equal "Reject patch"
                 (niwa-patch-review--primary-label
                  '(:operations ((:kind add :selected nil :content "x\n"))))))
  :doc "Counts selected changes, files, and staged comments"
  (should (equal "Apply 1 change in 1 file · send 1 comment"
                 (niwa-patch-review--primary-label
                  '(:operations
                    ((:kind update
                      :hunks ((:selected t :diff-lines ("-x" "+y"))
                              (:selected nil :feedback "no"
                               :diff-lines ("-a" "+b")))))))))
  :doc "Requests revision when only comments are staged"
  (should (equal "Request revision · 1 comment"
                 (niwa-patch-review--primary-label
                  '(:operations ((:kind add :selected nil :feedback "why"
                                  :content "x\n")))))))

(niwa-deftest niwa-patch-review--feedback-body
  (:doc "Renders staged feedback text") ,test (test)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (niwa-patch-review--feedback-body nil nil '(:feedback "note") "file")
      (should (string-search "note" (buffer-string))))))

(niwa-deftest niwa-patch-review--body
  (:doc "Builds the aggregate review summary") ,test (test)
  (should (string-search
           "ApplyPatch · 1 file"
           (niwa-patch-review--body
            '(:operations ((:kind add :rel-path "a" :content "x\n"
                            :selected t))))))

  :doc "Styles diff lines and shows the review controls"
  (let* ((body
          (niwa-patch-review--body
           '(:operations
             ((:kind update :rel-path "a.el"
               :expanded t
               :hunks ((:selected t :diff-lines ("-old" "+new"))))))))
         (removed (string-search "│ old" body))
         (added (string-search "│ new" body)))
    (should (member 'diff-removed
                    (get-text-property (+ removed 2) 'font-lock-face body)))
    (should (member 'niwa-patch-review-removed
                    (get-text-property (+ removed 2) 'font-lock-face body)))
    (should (member 'diff-added
                    (get-text-property (+ added 2) 'font-lock-face body)))
    (should (string-search "Keys:" body))
    (should (string-search "edit" body))
    (should (string-search "feedback" body)))

  :doc "File rows, hunk rows, and diff lines expose the same review keys"
  (with-temp-buffer
    (insert (niwa-patch-review--body
             '(:operations
               ((:kind update :rel-path "a.el" :expanded t
                 :hunks ((:selected t :diff-lines ("-old" "+new"))))))))
    (dolist (label '("M a.el" "@@" "│ old" "│ new"))
      (goto-char (point-min))
      (search-forward label)
      (dolist (binding '(("TAB" . niwa-patch-review-toggle-fold)
                         ("SPC" . niwa-patch-review-toggle-selection)
                         ("RET" . niwa-patch-review-visit)
                         ("<return>" . niwa-patch-review-visit)
                         ("n" . niwa-patch-review-next-row)
                         ("p" . niwa-patch-review-previous-row)
                         ("e" . niwa-patch-review-edit)
                         ("f" . niwa-patch-review-feedback)
                         ("C-c C-c" . niwa-patch-review-submit)
                         ("C-c C-k" . niwa-patch-review-reject)))
        (should (eq (cdr binding) (key-binding (kbd (car binding))))))))

  :doc "Dims a deselected hunk instead of tinting it"
  (let* ((body
          (niwa-patch-review--body
           '(:operations
             ((:kind update :rel-path "a.el"
               :expanded t
               :hunks ((:selected nil :diff-lines ("-old" "+new"))))))))
         (removed (string-search "│ old" body)))
    (should (equal '(shadow)
                   (get-text-property (+ removed 2) 'font-lock-face body)))
    (should (string-search "✗ @@" body)))

  :doc "Warns when review can race an active parent request"
  (let ((data-buffer (generate-new-buffer " *niwa-side-review*")))
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-side-conversation-parent-active-p)
                   (lambda (&optional buffer) (eq buffer data-buffer))))
          (should
           (string-search
            "parent request is still active"
            (niwa-patch-review--body
             `(:data-buffer ,data-buffer
               :operations
               ((:kind add :rel-path "a" :content "x\n"
                 :selected t)))))))
      (kill-buffer data-buffer))))

(niwa-deftest niwa-patch-review--render
  (:doc "Registers one preview interaction") ,test (test)
  (niwa-view-test--with-buffers
    (let ((proposal '(:id patch :operations nil)))
      (plist-put proposal :view-buffer view-buf)
      (with-current-buffer view-buf
        (should (overlayp (niwa-patch-review--render proposal)))
        ;; Cards stack by arrival; a producer carries no type priority.
        (let ((descriptor (gethash 'patch niwa-view--interaction-descriptors)))
          (should descriptor)
          (should-not (plist-member descriptor :priority)))))))

(niwa-deftest niwa-patch-review--sync-drafts
  (:doc "Captures live feedback drafts from field markers") ,test (test)
  (with-temp-buffer
    (insert "      note\n")
    (let* ((target (list :feedback-editing t
                         :feedback-start (copy-marker 7)
                         :feedback-end (copy-marker 11 t)))
           (proposal (list :operations
                           (list (list :hunks (list target))))))
      (niwa-patch-review--sync-drafts proposal)
      (should (equal "note" (plist-get target :feedback-draft))))))

(niwa-deftest niwa-patch-review--refresh-feedback-markers ()
  ,test
  (test)
  :doc "Recreates field markers from the rendered feedback field"
  (with-temp-buffer
    (let* ((target (list :feedback-editing t))
           (proposal (list :operations
                           (list (list :hunks (list target))))))
      (insert "      draft\n")
      (add-text-properties (point-min) (point-max)
                           (list 'niwa-patch-feedback-input target))
      (niwa-patch-review--refresh-feedback-markers proposal)
      (should (= 7 (marker-position (plist-get target :feedback-start))))
      (should (= 12 (marker-position (plist-get target :feedback-end))))))

  :doc "Cancels an edit whose field vanished from the rendering"
  (with-temp-buffer
    (let* ((target (list :feedback-editing t
                         :feedback "typed"
                         :feedback-original nil
                         :feedback-draft "typed"
                         :selected nil
                         :selected-original t))
           (proposal (list :operations
                           (list (list :hunks (list target))))))
      (niwa-patch-review--refresh-feedback-markers proposal)
      (should-not (plist-get target :feedback-editing))
      (should-not (plist-get target :feedback-draft))
      (should-not (plist-get target :feedback))
      (should (plist-get target :selected)))))

(niwa-deftest niwa-patch-review--edit-start
  (:doc "An ediff setup error releases its lock and temporary buffers")
  ,test
  (test)
  (niwa-view-test--with-buffers
    (let* ((path (make-temp-file "niwa-patch-edit-setup-"))
           (proposal (list :view-buffer view-buf))
           (operation (list :kind 'add :rel-path "setup.txt" :path path)))
      (unwind-protect
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (_buffer-a _buffer-b hooks &rest _)
                       (let ((control
                              (generate-new-buffer
                               " *niwa failing ediff control*")))
                         (unwind-protect
                             (with-current-buffer control
                               (mapc #'funcall hooks)
                               (should
                                (eq control
                                    (buffer-local-value
                                     'niwa-patch-review--edit-session
                                     view-buf)))
                               (error "Ediff setup failed"))
                           (kill-buffer control)))))
                    ((symbol-function
                      'niwa-patch-review--edit-goto-difference)
                     #'ignore))
            (should-error
             (niwa-patch-review--edit-start
              proposal operation "" "new\n" nil)
             :type 'error)
            (should-not (get-buffer "*niwa baseline: setup.txt*"))
            (should-not (get-buffer "*niwa result: setup.txt*"))
            (should-not
             (buffer-local-value
              'niwa-patch-review--edit-session view-buf)))
        (when (file-exists-p path) (delete-file path))))))

(niwa-deftest niwa-patch-review-toggle-fold
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'niwa-patch-review-toggle-fold)))

(niwa-deftest niwa-patch-review--feedback-input
  (:doc "Reads text between live feedback markers") ,test (test)
  (with-temp-buffer
    (let ((target (list :feedback-start (copy-marker (point-min)))))
      (insert "note")
      (plist-put target :feedback-end (copy-marker (point-max)))
      (add-text-properties (point-min) (point-max)
                           (list 'niwa-patch-feedback-input target))
      (goto-char (point-min))
      (should (equal "note" (cdr (niwa-patch-review--feedback-input)))))))

(niwa-deftest niwa-patch-review--clear-feedback-markers
  (:doc "Releases feedback markers") ,test (test)
  (with-temp-buffer
    (let ((target (list :feedback-start (copy-marker (point-min))
                        :feedback-end (copy-marker (point-min)))))
      (niwa-patch-review--clear-feedback-markers target)
      (should-not (plist-get target :feedback-start)))))

(niwa-deftest niwa-patch-review-confirm-feedback
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'niwa-patch-review-confirm-feedback)))

(niwa-deftest niwa-patch-review-cancel-feedback
  (:doc "Is an interactive review command") ,test (test)
  (should (commandp #'niwa-patch-review-cancel-feedback)))

(provide 'test-niwa-patch-review)
;;; test-niwa-patch-review.el ends here
