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

(defvar-local mevedel-cockpit-test--entries nil
  "Tabulated test entries.")

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
  (setq tabulated-list-format [("Name" 12 t)])
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun mevedel-cockpit-test--refresh ()
  "Refresh test cockpit rows."
  (mevedel-cockpit-refresh-tabulated mevedel-cockpit-test--entries))

(defun mevedel-cockpit-test--cleanup (&rest buffers)
  "Kill the test cockpit buffer and any live BUFFERS."
  (when (get-buffer mevedel-cockpit-test-buffer-name)
    (kill-buffer mevedel-cockpit-test-buffer-name))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun mevedel-cockpit-test--open
    (view-buffer data-buffer &optional origin-buffer entries)
  "Open the test cockpit owned by VIEW-BUFFER and DATA-BUFFER."
  (mevedel-cockpit-open-tabulated
   mevedel-cockpit-test-buffer-name
   #'mevedel-cockpit-test-mode
   #'mevedel-cockpit-test--refresh
   (mevedel-cockpit-test--context
    view-buffer data-buffer (or origin-buffer view-buffer))
   (lambda ()
     (setq mevedel-cockpit-test--entries
           (or entries
               '(("a" ["a"])
                 ("b" ["b"])))))
   "test cockpit"))

(mevedel-deftest mevedel-cockpit-open-tabulated ()
  ,test
  (test)

  :doc "opens a tabulated cockpit with live owners"
  (let ((view-buffer (generate-new-buffer " *cockpit-view*"))
        (data-buffer (generate-new-buffer " *cockpit-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (should (eq major-mode 'mevedel-cockpit-test-mode))
            (should (= 2 (length tabulated-list-entries)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer)))

  :doc "rejects dead owners before opening"
  (let ((view-buffer (generate-new-buffer " *cockpit-dead-open-view*"))
        (data-buffer (generate-new-buffer " *cockpit-dead-open-data*")))
    (unwind-protect
        (progn
          (kill-buffer data-buffer)
          (should-error
           (mevedel-cockpit-open-tabulated
            mevedel-cockpit-test-buffer-name
            #'mevedel-cockpit-test-mode
            #'mevedel-cockpit-test--refresh
            (list :view-buffer view-buffer
                  :data-buffer data-buffer
                  :origin-buffer view-buffer)
            nil
            "test cockpit")
           :type 'user-error))
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
        (let ((buffer (mevedel-cockpit-open-tabulated
                       mevedel-cockpit-test-buffer-name
                       #'mevedel-cockpit-test-mode
                       #'mevedel-cockpit-test--refresh
                       (mevedel-cockpit-test--context
                        view-buffer data-buffer data-buffer session)
                       (lambda ()
                         (setq mevedel-cockpit-test--entries
                               '(("a" ["a"]))))
                       "test cockpit")))
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

(mevedel-deftest mevedel-cockpit-refresh-tabulated ()
  ,test
  (test)

  :doc "preserves selected rows across refresh"
  (let ((view-buffer (generate-new-buffer " *cockpit-refresh-view*"))
        (data-buffer (generate-new-buffer " *cockpit-refresh-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (setq mevedel-cockpit-test--entries
                  '(("b" ["bee"])
                    ("c" ["cee"])))
            (mevedel-cockpit-test--refresh)
            (should (equal "b" (tabulated-list-get-id)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

(mevedel-deftest mevedel-cockpit-selected ()
  ,test
  (test)

  :doc "returns the item matching the selected row id"
  (let ((view-buffer (generate-new-buffer " *cockpit-selected-view*"))
        (data-buffer (generate-new-buffer " *cockpit-selected-data*")))
    (unwind-protect
        (let ((buffer (mevedel-cockpit-test--open view-buffer data-buffer)))
          (with-current-buffer buffer
            (mevedel-cockpit-goto-id "b")
            (should (equal "b"
                           (mevedel-cockpit-selected
                            '("a" "b")
                            #'identity)))))
      (mevedel-cockpit-test--cleanup view-buffer data-buffer))))

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
