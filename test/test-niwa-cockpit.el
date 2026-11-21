;;; test-niwa-cockpit.el --- Tests for niwa-cockpit.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-cockpit)
(require 'niwa-menu)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-workspace)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defconst niwa-cockpit-test-buffer-name "*niwa cockpit test*"
  "Name of the test cockpit buffer.")

(defvar-local niwa-cockpit-test--items nil
  "Test surface items.")

(defun niwa-cockpit-test--workspace ()
  "Return a test workspace."
  (niwa-workspace--create
   :type 'file :id "cockpit" :root "/tmp/cockpit" :name "cockpit"))

(defun niwa-cockpit-test--setup-pair (view-buffer data-buffer &optional session)
  "Wire VIEW-BUFFER and DATA-BUFFER as a cockpit pair."
  (with-current-buffer data-buffer
    (setq-local niwa--session session)
    (setq-local niwa--view-buffer view-buffer))
  (with-current-buffer view-buffer
    (niwa-view-mode)
    (setq-local niwa--data-buffer data-buffer)
    (setq-local niwa--session session)))

(defun niwa-cockpit-test--context
    (view-buffer data-buffer &optional origin-buffer session)
  "Return a cockpit context for VIEW-BUFFER and DATA-BUFFER."
  (niwa-cockpit-test--setup-pair view-buffer data-buffer session)
  (let ((context (copy-sequence
                  (niwa-cockpit-context-for-buffer view-buffer))))
    (plist-put context :origin-buffer (or origin-buffer view-buffer))))

(define-derived-mode niwa-cockpit-test-mode tabulated-list-mode
  "niwa-cockpit-test"
  "Test mode for `niwa-cockpit'."
  (niwa-cockpit-setup-tabulated-surface
   (niwa-cockpit-test--surface)))

(defun niwa-cockpit-test--collect (_context)
  "Return the test surface items."
  niwa-cockpit-test--items)

(defun niwa-cockpit-test--entry (item _context)
  "Return a tabulated row for ITEM."
  (list item (vector item "")))

(defun niwa-cockpit-test--header (items _context)
  "Return a test header for ITEMS."
  (format "test header: %d" (length items)))

(defun niwa-cockpit-test--details (item _context)
  "Return detail text for ITEM."
  (list :title "Details"
        :sections (list (list :id 'item :title "Item"
                              :body (format "details for %s" item)))))

(defconst niwa-cockpit-test--surface-spec
  `(:buffer-name ,niwa-cockpit-test-buffer-name
    :label "test cockpit"
    :row-label "test item"
    :mode niwa-cockpit-test-mode
    :format [("Name" 12 t) ("Note" 0 nil)]
    :sort-key ("Name" . nil)
    :collect niwa-cockpit-test--collect
    :entry niwa-cockpit-test--entry
    :header niwa-cockpit-test--header
    :details niwa-cockpit-test--details
    :details-buffer "*niwa cockpit test details*"
    :help-buffer "*niwa cockpit test help*"
    :help-report (:title "Test help"
                :sections ((:id keys :title "Keys" :body "test help")))
    :keys (("x" . ignore)
           ("y" "Run described test key" ignore)))
  "Surface spec used by cockpit tests.")

(defun niwa-cockpit-test--surface (&optional setup require-session)
  "Return a test surface with SETUP and REQUIRE-SESSION."
  (let ((surface (copy-sequence niwa-cockpit-test--surface-spec)))
    (when setup
      (setq surface (plist-put surface :setup setup)))
    (when require-session
      (setq surface (plist-put surface :require-session t)))
    surface))

(defun niwa-cockpit-test--cleanup (&rest buffers)
  "Kill the test cockpit buffer and any live BUFFERS."
  (when (get-buffer niwa-cockpit-test-buffer-name)
    (kill-buffer niwa-cockpit-test-buffer-name))
  (when (get-buffer "*niwa cockpit test details*")
    (kill-buffer "*niwa cockpit test details*"))
  (when (get-buffer "*niwa cockpit test help*")
    (kill-buffer "*niwa cockpit test help*"))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun niwa-cockpit-test--open
    (view-buffer data-buffer &optional origin-buffer entries)
  "Open the test cockpit owned by VIEW-BUFFER and DATA-BUFFER."
  (niwa-cockpit-open-surface
   (niwa-cockpit-test--surface
    (lambda (_context)
      (setq niwa-cockpit-test--items
            (or entries '("a" "b")))))
   (niwa-cockpit-test--context
    view-buffer data-buffer (or origin-buffer view-buffer))))

(niwa-deftest niwa-cockpit-open-surface ()
  ,test
  (test)

  :doc "opens a tabulated cockpit with live owners and generic keys"
  (let ((view-buffer (generate-new-buffer " *cockpit-view*"))
        (data-buffer (generate-new-buffer " *cockpit-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (eq major-mode 'niwa-cockpit-test-mode))
            (should (= 2 (length tabulated-list-entries)))
            (should (equal "test header: 2"
                           (niwa-cockpit-surface-header-line)))
            (should (eq (lookup-key (current-local-map) (kbd "g"))
                        #'niwa-cockpit-surface-refresh))
            (should (eq (lookup-key (current-local-map) (kbd "?"))
                        #'niwa-cockpit-surface-help))
            (should (eq (lookup-key (current-local-map) (kbd "q"))
                        #'niwa-cockpit-surface-quit))
            (should (eq (lookup-key (current-local-map) (kbd "RET"))
                        #'niwa-cockpit-surface-details))
            (should (eq (lookup-key (current-local-map) (kbd "x"))
                        #'ignore))
            (should (eq (lookup-key (current-local-map) (kbd "y"))
                        #'ignore))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "opening for another session releases the surface the first owns"
  ;; The buffer name is a constant, so reuse used to overwrite the
  ;; context -- and with it the only handle on whatever the surface still
  ;; owed the previous session.
  (let ((first-view (generate-new-buffer " *cockpit-reuse-view-1*"))
        (first-data (generate-new-buffer " *cockpit-reuse-data-1*"))
        (second-view (generate-new-buffer " *cockpit-reuse-view-2*"))
        (second-data (generate-new-buffer " *cockpit-reuse-data-2*"))
        (workspace (niwa-cockpit-test--workspace)))
    (unwind-protect
        (let* ((first-session (niwa-session-create "first" workspace))
               (second-session (niwa-session-create "second" workspace))
               (first-buffer
                (niwa-cockpit-open-surface
                 (niwa-cockpit-test--surface)
                 (niwa-cockpit-test--context
                  first-view first-data first-view first-session)))
               (second-buffer
                (niwa-cockpit-open-surface
                 (niwa-cockpit-test--surface)
                 (niwa-cockpit-test--context
                  second-view second-data second-view second-session))))
          (should-not (buffer-live-p first-buffer))
          (should (buffer-live-p second-buffer))
          ;; Reopening for the same session reuses its own buffer.
          (should (eq second-buffer
                      (niwa-cockpit-open-surface
                       (niwa-cockpit-test--surface)
                       (niwa-cockpit-test--context
                        second-view second-data second-view
                        second-session)))))
      (niwa-cockpit-test--cleanup first-view first-data)
      (niwa-cockpit-test--cleanup second-view second-data)))

  :doc "rejects dead owners before opening"
  (let ((view-buffer (generate-new-buffer " *cockpit-dead-open-view*"))
        (data-buffer (generate-new-buffer " *cockpit-dead-open-data*")))
    (unwind-protect
        (progn
          (kill-buffer data-buffer)
          (should-error
           (niwa-cockpit-open-surface
            (niwa-cockpit-test--surface)
            (list :view-buffer view-buffer
                  :data-buffer data-buffer
                  :origin-buffer view-buffer))
           :type 'user-error))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "rejects missing sessions when the surface requires one"
  (let ((view-buffer (generate-new-buffer " *cockpit-session-view*"))
        (data-buffer (generate-new-buffer " *cockpit-session-data*")))
    (unwind-protect
        (should-error
         (niwa-cockpit-open-surface
          (niwa-cockpit-test--surface nil t)
          (niwa-cockpit-test--context view-buffer data-buffer))
         :type 'user-error)
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-current-context ()
  ,test
  (test)

  :doc "resolves a full context from a view buffer"
  (let* ((workspace (niwa-cockpit-test--workspace))
         (session (niwa-session-create "main" workspace))
         (view-buffer (generate-new-buffer " *cockpit-context-view*"))
         (data-buffer (generate-new-buffer " *cockpit-context-data*")))
    (unwind-protect
        (progn
          (niwa-cockpit-test--setup-pair
           view-buffer data-buffer session)
          (with-current-buffer view-buffer
            (let ((context (niwa-cockpit-current-context)))
              (should (eq (niwa-cockpit-context-view-buffer context)
                          view-buffer))
              (should (eq (niwa-cockpit-context-data-buffer context)
                          data-buffer))
              (should (eq (niwa-cockpit-context-origin-buffer context)
                          view-buffer))
              (should (eq (niwa-cockpit-context-session context)
                          session))
              (should (eq (niwa-cockpit-context-workspace context)
                          workspace)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "resolves a full context from a data buffer"
  (let* ((workspace (niwa-cockpit-test--workspace))
         (session (niwa-session-create "main" workspace))
         (view-buffer (generate-new-buffer " *cockpit-context-view*"))
         (data-buffer (generate-new-buffer " *cockpit-context-data*")))
    (unwind-protect
        (progn
          (niwa-cockpit-test--setup-pair
           view-buffer data-buffer session)
          (with-current-buffer data-buffer
            (let ((context (niwa-cockpit-current-context)))
              (should (eq (niwa-cockpit-context-view-buffer context)
                          view-buffer))
              (should (eq (niwa-cockpit-context-data-buffer context)
                          data-buffer))
              (should (eq (niwa-cockpit-context-origin-buffer context)
                          data-buffer))
              (should (eq (niwa-cockpit-context-session context)
                          session))
              (should (eq (niwa-cockpit-context-workspace context)
                          workspace)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "resolves the stored context from a tabulated cockpit buffer"
  (let* ((workspace (niwa-cockpit-test--workspace))
         (session (niwa-session-create "main" workspace))
         (view-buffer (generate-new-buffer " *cockpit-context-view*"))
         (data-buffer (generate-new-buffer " *cockpit-context-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-open-surface
                       (niwa-cockpit-test--surface
                        (lambda (_context)
                          (setq niwa-cockpit-test--items '("a"))))
                       (niwa-cockpit-test--context
                        view-buffer data-buffer data-buffer session))))
          (with-current-buffer buffer
            (let ((context (niwa-cockpit-current-context)))
              (should (eq (niwa-cockpit-context-view-buffer context)
                          view-buffer))
              (should (eq (niwa-cockpit-context-data-buffer context)
                          data-buffer))
              (should (eq (niwa-cockpit-context-origin-buffer context)
                          data-buffer))
              (should (eq (niwa-cockpit-context-session context)
                          session))
              (should (eq (niwa-cockpit-context-workspace context)
                          workspace)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-require-owner ()
  ,test
  (test)

  :doc "accepts live owners and rejects dead owners"
  (let ((view-buffer (generate-new-buffer " *cockpit-owner-view*"))
        (data-buffer (generate-new-buffer " *cockpit-owner-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (niwa-cockpit-require-owner "test cockpit"))
            (kill-buffer data-buffer)
            (should-error (niwa-cockpit-require-owner "test cockpit")
                          :type 'user-error)))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "attributes a dead owner's refusal to its explicit workspace"
  (with-temp-buffer
    (let* ((workspace (niwa-cockpit-test--workspace))
           (context (list :workspace workspace :data-buffer nil :view-buffer nil))
           (err (should-error (niwa-cockpit-require-owner "test cockpit" context)
                              :type 'user-error)))
      (should (equal (error-message-string err)
                     "niwa [/tmp/cockpit]: No live niwa session for this test cockpit"))))

  :doc "accepts a workspace context, which has no owners that can die"
  (let ((workspace (niwa-workspace--create :type 'project :id "w")))
    (should (niwa-cockpit-require-owner
             "test cockpit" (niwa-cockpit-workspace-context workspace)))
    (should-error (niwa-cockpit-require-owner
                   "test cockpit" (niwa-cockpit-workspace-context nil))
                  :type 'user-error)))

(niwa-deftest niwa-cockpit-diagnostic-scope ()
  ,test (test)
  :doc "prefers explicit context over the current buffer and never discovers"
  (with-temp-buffer
    (let* ((workspace (niwa-cockpit-test--workspace))
           (session (niwa-session--create :workspace workspace)))
      (cl-letf (((symbol-function 'niwa-workspace)
                 (lambda (&rest _) (ert-fail "Diagnostic discovered a workspace"))))
        (should (equal "/tmp/cockpit"
                       (niwa-cockpit-diagnostic-scope (list :session session))))
        (should (eq 'unavailable (niwa-cockpit-diagnostic-scope)))))))

(niwa-deftest niwa-cockpit-workspace-context ()
  ,test
  (test)

  :doc "carries the workspace and origin but no session"
  (let* ((workspace (niwa-workspace--create :type 'project :id "w"))
         (context (niwa-cockpit-workspace-context workspace 'origin)))
    (should (eq workspace (niwa-cockpit-context-workspace context)))
    (should (eq 'origin (plist-get context :origin-buffer)))
    (should-not (niwa-cockpit-context-session context))
    (should (niwa-cockpit--live-context-p context))))

(niwa-deftest niwa-cockpit-data-buffer ()
  ,test
  (test)

  :doc "returns the live data owner for the current cockpit"
  (let ((view-buffer (generate-new-buffer " *cockpit-data-view*"))
        (data-buffer (generate-new-buffer " *cockpit-data-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (eq (niwa-cockpit-data-buffer) data-buffer))
            (kill-buffer data-buffer)
            (should-not (niwa-cockpit-data-buffer))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-goto-id ()
  ,test
  (test)

  :doc "moves to a matching row id or the first row"
  (let ((view-buffer (generate-new-buffer " *cockpit-goto-view*"))
        (data-buffer (generate-new-buffer " *cockpit-goto-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id "b")
            (should (equal "b" (tabulated-list-get-id)))
            (niwa-cockpit-goto-id "missing")
            (should (equal "a" (tabulated-list-get-id)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-surface-refresh ()
  ,test
  (test)

  :doc "preserves selected rows across refresh"
  (let ((view-buffer (generate-new-buffer " *cockpit-refresh-view*"))
        (data-buffer (generate-new-buffer " *cockpit-refresh-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id "b")
            (setq niwa-cockpit-test--items '("b" "c"))
            (niwa-cockpit-surface-refresh)
            (should (equal "b" (tabulated-list-get-id)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))
  :doc "preserves the cursor column across live refresh"
  (let ((view-buffer (generate-new-buffer " *cockpit-refresh-point-view*"))
        (data-buffer (generate-new-buffer " *cockpit-refresh-point-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            ;; Batch has no redisplay to run the fit on display.
            (niwa-cockpit-surface-refresh)
            (niwa-cockpit-goto-id "b")
            (end-of-line)
            (let ((column (current-column)))
              (niwa-cockpit-surface-refresh)
              (should (= column (current-column))))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit--fit-format ()
  ,test
  (test)

  :doc "shrinks a column wider than its cells to its sortable header"
  (should (equal [("Status" 8 t) ("Rest" 0 t)]
                 (niwa-cockpit--fit-format
                  [("Status" 20 t) ("Rest" 0 t)]
                  '((a ["indexed" "x"])) 2 80)))
  :doc "grows a truncated column into unused width"
  (should (equal [("Name" 13 t :pad-right 2) ("Rest" 0 nil)]
                 (niwa-cockpit--fit-format
                  [("Name" 6 t :pad-right 2) ("Rest" 0 nil)]
                  `((a [,(make-string 13 ?n) "x"])) 2 80)))
  :doc "measures button and image cells"
  (should (equal [("A" 5 nil) ("B" 1 nil) ("C" 0 nil)]
                 (niwa-cockpit--fit-format
                  [("A" 10 nil) ("B" 10 nil) ("C" 0 nil)]
                  '((a [("click" action ignore) (image :type png) "x"]))
                  0 80)))
  :doc "keeps a third of the line for a long last column"
  ;; 2 padding + (4 + 1) + (14 + 1) = 22 used and (38 - 2) / 3 = 12 kept
  ;; for the last column leave 38 - 22 - 12 = 4 for Title to grow.
  (should (equal [("Id" 4 nil) ("Title" 18 nil) ("Path" 0 nil)]
                 (niwa-cockpit--fit-format
                  [("Id" 4 nil) ("Title" 14 nil) ("Path" 0 nil)]
                  `((a ["1234" ,(make-string 40 ?t) ,(make-string 36 ?p)]))
                  2 38)))
  :doc "keeps declared widths when the window is too narrow"
  (should (equal [("Title" 10 nil) ("Path" 0 nil)]
                 (niwa-cockpit--fit-format
                  [("Title" 10 nil) ("Path" 0 nil)]
                  `((a [,(make-string 40 ?t) ,(make-string 40 ?p)]))
                  2 12))))

(niwa-deftest niwa-cockpit--refit ()
  ,test
  (test)

  :doc "fits a displayed cockpit to its window and reprints on resize"
  (let ((view-buffer (generate-new-buffer " *cockpit-refit-view*"))
        (data-buffer (generate-new-buffer " *cockpit-refit-data*")))
    (unwind-protect
        (save-window-excursion
          (let* ((long (make-string 30 ?n))
                 (buffer (niwa-cockpit-test--open
                          view-buffer data-buffer nil (list long "b")))
                 (window (selected-window)))
            (delete-other-windows window)
            (with-current-buffer buffer
              (should (memq #'niwa-cockpit--refit
                            window-size-change-functions))
              (niwa-cockpit--refit window)
              (should (equal [("Name" 30 t) ("Note" 0 nil)]
                             tabulated-list-format))
              (goto-char (point-min))
              (should (search-forward long nil t))
              (should-not (niwa-cockpit--fit-columns window)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-surface-selected ()
  ,test
  (test)

  :doc "returns the item matching the selected row id"
  (let ((view-buffer (generate-new-buffer " *cockpit-selected-view*"))
        (data-buffer (generate-new-buffer " *cockpit-selected-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id "b")
            (should (equal "b" (niwa-cockpit-surface-selected)))
            (niwa-cockpit-goto-id "missing")
            (should (equal "a" (tabulated-list-get-id)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-surface-details ()
  ,test
  (test)

  :doc "renders details for the selected row through the surface spec"
  (let ((view-buffer (generate-new-buffer " *cockpit-details-view*"))
        (data-buffer (generate-new-buffer " *cockpit-details-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-goto-id "b")
            (niwa-cockpit-surface-details))
          (with-current-buffer "*niwa cockpit test details*"
            (should (string-match-p "details for b" (buffer-string)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-surface-help ()
  ,test
  (test)

  :doc "renders help through the surface spec"
  (let ((view-buffer (generate-new-buffer " *cockpit-help-view*"))
        (data-buffer (generate-new-buffer " *cockpit-help-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (niwa-cockpit-surface-help))
          (with-current-buffer "*niwa cockpit test help*"
            (should (string-match-p "test help" (buffer-string)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "falls back to generated key help for a surface without help text"
  (let ((view-buffer (generate-new-buffer " *cockpit-help-view*"))
        (data-buffer (generate-new-buffer " *cockpit-help-data*"))
        (surface (plist-put (copy-sequence niwa-cockpit-test--surface-spec)
					   :help-report nil)))
    (unwind-protect
        (let ((buffer (niwa-cockpit-open-surface
                       (plist-put surface :setup
                                  (lambda (_context)
                                    (setq niwa-cockpit-test--items
                                          '("a" "b"))))
                       (niwa-cockpit-test--context
                        view-buffer data-buffer view-buffer))))
          (with-current-buffer buffer
            (niwa-cockpit-surface-help))
          (with-current-buffer "*niwa cockpit test help*"
            (let ((text (buffer-string)))
              (should (string-match-p "niwa test cockpit" text))
              (should (string-match-p "RET  Show selected test item details"
                                      text))
              (should (string-match-p "y    Run described test key" text)))))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-sort-by-number
  (:doc "orders rows by the numbers behind their cells, not their text")
  (let ((rows (mapcar (lambda (size)
                        (list size (vector (niwa-cockpit-number-cell
                                            (file-size-human-readable size)
                                            size))))
                      '(204800 980 1572864))))
    (should (equal '(980 204800 1572864)
                   (mapcar #'car (sort rows (niwa-cockpit-sort-by-number 0)))))))

(niwa-deftest niwa-cockpit-format-header ()
  ,test
  (test)

  :doc "shows identity, scope, state, and the pointer to the keys"
  (let ((header (substring-no-properties
                 (niwa-cockpit-format-header
                  "tools" "main" "24 active"))))
    (should (string-prefix-p "niwa: tools" header))
    (should (string-match-p "main" header))
    (should (string-match-p "24 active" header))
    (should (string-suffix-p "? keys" header))))

(niwa-deftest niwa-cockpit-surface-key-help-text ()
  ,test
  (test)

  :doc "renders default and described surface keys"
  (let ((text (niwa-cockpit-surface-key-help-text
               niwa-cockpit-test--surface-spec)))
    (should (string-match-p "RET  Show selected test item details" text))
    (should (string-match-p "g    Refresh table" text))
    (should (string-match-p "y    Run described test key" text))
    (should-not (string-match-p "x    " text))))

(niwa-deftest niwa-cockpit-quit ()
  ,test
  (test)

  :doc "kills a directly opened cockpit without reopening the menu"
  (dolist (case '((origin)
                  (view kill-origin)
                  (data kill-origin kill-view)))
    (let ((view-buffer (generate-new-buffer " *cockpit-quit-view*"))
          (data-buffer (generate-new-buffer " *cockpit-quit-data*"))
          (origin-buffer (generate-new-buffer " *cockpit-quit-origin*"))
          called-buffer)
      (unwind-protect
          (let ((buffer (niwa-cockpit-test--open
                         view-buffer data-buffer origin-buffer nil)))
            (when (memq 'kill-origin case)
              (kill-buffer origin-buffer))
            (when (memq 'kill-view case)
              (kill-buffer view-buffer))
            (cl-letf (((symbol-function 'niwa-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (with-current-buffer buffer
                (niwa-cockpit-quit "test cockpit")))
            (should-not (buffer-live-p buffer))
            (should-not called-buffer))
        (niwa-cockpit-test--cleanup
         view-buffer data-buffer origin-buffer))))

  :doc "quit remains available after the owners die"
  (let ((view-buffer (generate-new-buffer " *cockpit-dead-view*"))
        (data-buffer (generate-new-buffer " *cockpit-dead-data*")))
    (unwind-protect
        (let ((buffer (niwa-cockpit-test--open
                       view-buffer data-buffer view-buffer nil)))
          (kill-buffer view-buffer)
          (kill-buffer data-buffer)
          (with-current-buffer buffer
            (niwa-cockpit-quit "test cockpit"))
          (should-not (buffer-live-p buffer)))
      (niwa-cockpit-test--cleanup view-buffer data-buffer))))

(niwa-deftest niwa-cockpit-menu-return-function ()
  ,test
  (test)

  :doc "inherited ownership never makes a direct open return to the menu"
  (let ((view (generate-new-buffer " *return-function-view*"))
        (data (generate-new-buffer " *return-function-data*"))
        called)
    (unwind-protect
        (let* ((context (niwa-cockpit-test--context view data))
               (context (plist-put context :return-menu (lambda () (setq called t)))))
          (let ((transient-current-command nil))
            (should-not (niwa-cockpit-menu-return-function context)))
          (let ((transient-current-command 'gptel-menu))
            (should-not (niwa-cockpit-menu-return-function context)))
          (let ((transient-current-command 'niwa-menu--top))
            (funcall (niwa-cockpit-menu-return-function context)))
          (should called))
      (niwa-cockpit-test--cleanup view data))))

(provide 'test-niwa-cockpit)

;;; test-niwa-cockpit.el ends here
