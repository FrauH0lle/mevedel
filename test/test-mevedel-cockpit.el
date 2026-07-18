;;; test-mevedel-cockpit.el --- Tests for mevedel-cockpit.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-cockpit)
(require 'mevedel-menu)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defconst mevedel-cockpit-test-buffer-name "*mevedel cockpit test*"
  "Name of the test cockpit buffer.")

(defvar-local mevedel-cockpit-test--items nil
  "Test surface items.")

(defun mevedel-cockpit-test--workspace ()
  "Return a test workspace."
  (mevedel-workspace--create
   :type 'test :id "cockpit" :root "/tmp/cockpit" :name "cockpit"))

(defun mevedel-cockpit-test--setup-pair (view-buffer data-buffer &optional session)
  "Wire VIEW-BUFFER and DATA-BUFFER as a cockpit pair."
  (with-current-buffer data-buffer
    (setq-local mevedel--session session)
    (setq-local mevedel--view-buffer view-buffer))
  (with-current-buffer view-buffer
    (mevedel-view-mode)
    (setq-local mevedel--data-buffer data-buffer)
    (setq-local mevedel--session session)))

(defun mevedel-cockpit-test--context
    (view-buffer data-buffer &optional origin-buffer session)
  "Return a cockpit context for VIEW-BUFFER and DATA-BUFFER."
  (mevedel-cockpit-test--setup-pair view-buffer data-buffer session)
  (let ((context (copy-sequence
                  (mevedel-cockpit-context-for-buffer view-buffer))))
    (plist-put context :origin-buffer (or origin-buffer view-buffer))))

(define-derived-mode mevedel-cockpit-test-mode tabulated-list-mode
  "mevedel-cockpit-test"
  "Test mode for `mevedel-cockpit'."
  (mevedel-cockpit-setup-tabulated-surface
   (mevedel-cockpit-test--surface)))

(defun mevedel-cockpit-test--collect (_context)
  "Return the test surface items."
  mevedel-cockpit-test--items)

(defun mevedel-cockpit-test--entry (item _context)
  "Return a tabulated row for ITEM."
  (list item (vector item)))

(defun mevedel-cockpit-test--header (items _context)
  "Return a test header for ITEMS."
  (format "test header: %d" (length items)))

(defun mevedel-cockpit-test--details (item _context)
  "Return detail text for ITEM."
  (format "details for %s" item))

(defconst mevedel-cockpit-test--surface-spec
  `(:buffer-name ,mevedel-cockpit-test-buffer-name
    :label "test cockpit"
    :row-label "test item"
    :mode mevedel-cockpit-test-mode
    :format [("Name" 12 t)]
    :sort-key ("Name" . nil)
    :collect mevedel-cockpit-test--collect
    :entry mevedel-cockpit-test--entry
    :header mevedel-cockpit-test--header
    :details mevedel-cockpit-test--details
    :details-buffer "*mevedel cockpit test details*"
    :help-buffer "*mevedel cockpit test help*"
    :help-text "test help"
    :keys (("x" . ignore)
           ("y" "Run described test key" ignore)))
  "Surface spec used by cockpit tests.")

(defun mevedel-cockpit-test--surface (&optional setup require-session)
  "Return a test surface with SETUP and REQUIRE-SESSION."
  (let ((surface (copy-sequence mevedel-cockpit-test--surface-spec)))
    (when setup
      (setq surface (plist-put surface :setup setup)))
    (when require-session
      (setq surface (plist-put surface :require-session t)))
    surface))

(defun mevedel-cockpit-test--cleanup (&rest buffers)
  "Kill the test cockpit buffer and any live BUFFERS."
  (when (get-buffer mevedel-cockpit-test-buffer-name)
    (kill-buffer mevedel-cockpit-test-buffer-name))
  (when (get-buffer "*mevedel cockpit test details*")
    (kill-buffer "*mevedel cockpit test details*"))
  (when (get-buffer "*mevedel cockpit test help*")
    (kill-buffer "*mevedel cockpit test help*"))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun mevedel-cockpit-test--open
    (view-buffer data-buffer &optional origin-buffer entries)
  "Open the test cockpit owned by VIEW-BUFFER and DATA-BUFFER."
  (mevedel-cockpit-open-surface
   (mevedel-cockpit-test--surface
    (lambda (_context)
      (setq mevedel-cockpit-test--items
            (or entries '("a" "b")))))
   (mevedel-cockpit-test--context
    view-buffer data-buffer (or origin-buffer view-buffer))))

(mevedel-deftest mevedel-cockpit-open-surface ()
  ,test
  (test)

  :doc "opens a tabulated cockpit with live owners and generic keys"
  (let ((view-buffer (generate-new-buffer " *cockpit-view*"))
        (data-buffer (generate-new-buffer " *cockpit-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (eq major-mode 'mevedel-cockpit-test-mode))
            (should (= 2 (length tabulated-list-entries)))
            (should (equal "test header: 2"
                           (mevedel-cockpit-surface-header-line)))
            (should (eq (lookup-key (current-local-map) (kbd "g"))
                        #'mevedel-cockpit-surface-refresh))
            (should (eq (lookup-key (current-local-map) (kbd "?"))
                        #'mevedel-cockpit-surface-help))
            (should (eq (lookup-key (current-local-map) (kbd "q"))
                        #'mevedel-cockpit-surface-quit))
            (should (eq (lookup-key (current-local-map) (kbd "RET"))
                        #'mevedel-cockpit-surface-details))
            (should (eq (lookup-key (current-local-map) (kbd "x"))
                        #'ignore))
            (should (eq (lookup-key (current-local-map) (kbd "y"))
                        #'ignore))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "rejects dead owners before opening"
  (let ((view-buffer (generate-new-buffer " *cockpit-dead-open-view*"))
        (data-buffer (generate-new-buffer " *cockpit-dead-open-data*")))
    (unwind-protect
        (progn
          (kill-buffer data-buffer)
          (should-error
           (mevedel-cockpit-open-surface
            (mevedel-cockpit-test--surface)
            (list :view-buffer view-buffer
                  :data-buffer data-buffer
                  :origin-buffer view-buffer))
           :type 'user-error))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "rejects missing sessions when the surface requires one"
  (let ((view-buffer (generate-new-buffer " *cockpit-session-view*"))
        (data-buffer (generate-new-buffer " *cockpit-session-data*")))
    (unwind-protect
        (should-error
         (mevedel-cockpit-open-surface
          (mevedel-cockpit-test--surface nil t)
          (mevedel-cockpit-test--context view-buffer data-buffer))
         :type 'user-error)
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-current-context ()
  ,test
  (test)

  :doc "resolves a full context from a view buffer"
  (let* ((workspace (mevedel-cockpit-test--workspace))
         (session (mevedel-session-create "main" workspace))
         (view-buffer (generate-new-buffer " *cockpit-context-view*"))
         (data-buffer (generate-new-buffer " *cockpit-context-data*")))
    (unwind-protect
        (progn
          (mevedel-cockpit-test--setup-pair
           view-buffer data-buffer session)
          (with-current-buffer view-buffer
            (let ((context (mevedel-cockpit-current-context)))
              (should (eq (mevedel-cockpit-context-view-buffer context)
                          view-buffer))
              (should (eq (mevedel-cockpit-context-data-buffer context)
                          data-buffer))
              (should (eq (mevedel-cockpit-context-origin-buffer context)
                          view-buffer))
              (should (eq (mevedel-cockpit-context-session context)
                          session))
              (should (eq (mevedel-cockpit-context-workspace context)
                          workspace)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "resolves a full context from a data buffer"
  (let* ((workspace (mevedel-cockpit-test--workspace))
         (session (mevedel-session-create "main" workspace))
         (view-buffer (generate-new-buffer " *cockpit-context-view*"))
         (data-buffer (generate-new-buffer " *cockpit-context-data*")))
    (unwind-protect
        (progn
          (mevedel-cockpit-test--setup-pair
           view-buffer data-buffer session)
          (with-current-buffer data-buffer
            (let ((context (mevedel-cockpit-current-context)))
              (should (eq (mevedel-cockpit-context-view-buffer context)
                          view-buffer))
              (should (eq (mevedel-cockpit-context-data-buffer context)
                          data-buffer))
              (should (eq (mevedel-cockpit-context-origin-buffer context)
                          data-buffer))
              (should (eq (mevedel-cockpit-context-session context)
                          session))
              (should (eq (mevedel-cockpit-context-workspace context)
                          workspace)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "resolves the stored context from a tabulated cockpit buffer"
  (let* ((workspace (mevedel-cockpit-test--workspace))
         (session (mevedel-session-create "main" workspace))
         (view-buffer (generate-new-buffer " *cockpit-context-view*"))
         (data-buffer (generate-new-buffer " *cockpit-context-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-open-surface
                       (mevedel-cockpit-test--surface
                        (lambda (_context)
                          (setq mevedel-cockpit-test--items '("a"))))
                       (mevedel-cockpit-test--context
                        view-buffer data-buffer data-buffer session))))
          (with-current-buffer buffer
            (let ((context (mevedel-cockpit-current-context)))
              (should (eq (mevedel-cockpit-context-view-buffer context)
                          view-buffer))
              (should (eq (mevedel-cockpit-context-data-buffer context)
                          data-buffer))
              (should (eq (mevedel-cockpit-context-origin-buffer context)
                          data-buffer))
              (should (eq (mevedel-cockpit-context-session context)
                          session))
              (should (eq (mevedel-cockpit-context-workspace context)
                          workspace)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-require-owner ()
  ,test
  (test)

  :doc "accepts live owners and rejects dead owners"
  (let ((view-buffer (generate-new-buffer " *cockpit-owner-view*"))
        (data-buffer (generate-new-buffer " *cockpit-owner-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (mevedel-cockpit-require-owner "test cockpit"))
            (kill-buffer data-buffer)
            (should-error (mevedel-cockpit-require-owner "test cockpit")
                          :type 'user-error)))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-data-buffer ()
  ,test
  (test)

  :doc "returns the live data owner for the current cockpit"
  (let ((view-buffer (generate-new-buffer " *cockpit-data-view*"))
        (data-buffer (generate-new-buffer " *cockpit-data-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (eq (mevedel-cockpit-data-buffer) data-buffer))
            (kill-buffer data-buffer)
            (should-not (mevedel-cockpit-data-buffer))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-goto-id ()
  ,test
  (test)

  :doc "moves to a matching row id or the first row"
  (let ((view-buffer (generate-new-buffer " *cockpit-goto-view*"))
        (data-buffer (generate-new-buffer " *cockpit-goto-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (should (equal "b" (tabulated-list-get-id)))
            (mevedel-cockpit-goto-id "missing")
            (should (equal "a" (tabulated-list-get-id)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-surface-refresh ()
  ,test
  (test)

  :doc "preserves selected rows across refresh"
  (let ((view-buffer (generate-new-buffer " *cockpit-refresh-view*"))
        (data-buffer (generate-new-buffer " *cockpit-refresh-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (setq mevedel-cockpit-test--items '("b" "c"))
            (mevedel-cockpit-surface-refresh)
            (should (equal "b" (tabulated-list-get-id)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer)))
  :doc "preserves the cursor column across live refresh"
  (let ((view-buffer (generate-new-buffer " *cockpit-refresh-point-view*"))
        (data-buffer (generate-new-buffer " *cockpit-refresh-point-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (end-of-line)
            (let ((column (current-column)))
              (mevedel-cockpit-surface-refresh)
              (should (= column (current-column))))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-surface-selected ()
  ,test
  (test)

  :doc "returns the item matching the selected row id"
  (let ((view-buffer (generate-new-buffer " *cockpit-selected-view*"))
        (data-buffer (generate-new-buffer " *cockpit-selected-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (should (equal "b" (mevedel-cockpit-surface-selected)))
            (mevedel-cockpit-goto-id "missing")
            (should (equal "a" (tabulated-list-get-id)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-surface-details ()
  ,test
  (test)

  :doc "renders details for the selected row through the surface spec"
  (let ((view-buffer (generate-new-buffer " *cockpit-details-view*"))
        (data-buffer (generate-new-buffer " *cockpit-details-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (mevedel-cockpit-surface-details))
          (with-current-buffer "*mevedel cockpit test details*"
            (should (string-match-p "details for b" (buffer-string)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-surface-help ()
  ,test
  (test)

  :doc "renders help through the surface spec"
  (let ((view-buffer (generate-new-buffer " *cockpit-help-view*"))
        (data-buffer (generate-new-buffer " *cockpit-help-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-surface-help))
          (with-current-buffer "*mevedel cockpit test help*"
            (should (string-match-p "test help" (buffer-string)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-surface-key-help-text ()
  ,test
  (test)

  :doc "renders default and described surface keys"
  (let ((text (mevedel-cockpit-surface-key-help-text
               mevedel-cockpit-test--surface-spec)))
    (should (string-match-p "RET  Show selected test item details" text))
    (should (string-match-p "g    Refresh table" text))
    (should (string-match-p "y    Run described test key" text))
    (should-not (string-match-p "x    " text))))

(mevedel-deftest mevedel-cockpit--return-buffer ()
  ,test
  (test)

  :doc "returns origin, then view, then data as owners disappear"
  (let ((view-buffer (generate-new-buffer " *cockpit-return-view*"))
        (data-buffer (generate-new-buffer " *cockpit-return-data*"))
        (origin-buffer (generate-new-buffer " *cockpit-return-origin*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open
                       view-buffer data-buffer origin-buffer nil)))
          (with-current-buffer buffer
            (should (eq (mevedel-cockpit--return-buffer) origin-buffer))
            (kill-buffer origin-buffer)
            (should (eq (mevedel-cockpit--return-buffer) view-buffer))
            (kill-buffer view-buffer)
            (should (eq (mevedel-cockpit--return-buffer) data-buffer))
            (kill-buffer data-buffer)
            (should-not (mevedel-cockpit--return-buffer))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer origin-buffer))))

(mevedel-deftest mevedel-cockpit-quit ()
  ,test
  (test)

  :doc "kills the cockpit and returns through origin, view, then data"
  (dolist (case '((origin)
                  (view kill-origin)
                  (data kill-origin kill-view)))
    (let ((view-buffer (generate-new-buffer " *cockpit-quit-view*"))
          (data-buffer (generate-new-buffer " *cockpit-quit-data*"))
          (origin-buffer (generate-new-buffer " *cockpit-quit-origin*"))
          called-buffer)
      (unwind-protect
          (let ((buffer (mevedel-cockpit-test--open
                         view-buffer data-buffer origin-buffer nil)))
            (when (memq 'kill-origin case)
              (kill-buffer origin-buffer))
            (when (memq 'kill-view case)
              (kill-buffer view-buffer))
            (cl-letf (((symbol-function 'mevedel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (with-current-buffer buffer
                (mevedel-cockpit-quit "test cockpit")))
            (should-not (buffer-live-p buffer))
            (should (eq called-buffer
                        (pcase (car case)
                          ('origin origin-buffer)
                          ('view view-buffer)
                          ('data data-buffer)))))
        (mevedel-cockpit-test--cleanup
         view-buffer data-buffer origin-buffer))))

  :doc "quit kills the cockpit before reporting dead owners"
  (let ((view-buffer (generate-new-buffer " *cockpit-dead-view*"))
        (data-buffer (generate-new-buffer " *cockpit-dead-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open
                       view-buffer data-buffer view-buffer nil)))
          (kill-buffer view-buffer)
          (kill-buffer data-buffer)
          (with-current-buffer buffer
            (should-error (mevedel-cockpit-quit "test cockpit")
                          :type 'user-error))
          (should-not (buffer-live-p buffer)))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(provide 'test-mevedel-cockpit)

;;; test-mevedel-cockpit.el ends here
