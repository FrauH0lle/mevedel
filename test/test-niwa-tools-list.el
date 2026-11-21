;;; test-niwa-tools-list.el --- Tests for niwa-tools-list.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))

(require 'gptel)
(require 'gptel-request)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'niwa-tool-registry)
(require 'niwa-agents)
(require 'niwa-tools)
(require 'niwa-tools-list)
(require 'niwa-cockpit)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar gptel--known-tools)


;;
;;; Helpers

(defun niwa-tools-list-test--make-session ()
  "Create a fresh tools-list-test session."
  (let ((ws (niwa-workspace-get-or-create
             'project "/tmp/mt/" "/tmp/mt/" "mt")))
    (niwa-session-create "main" ws)))

(defun niwa-tools-list-test--make-fake-gptel-tool (name &optional category)
  "Return a minimal `gptel-tool' with NAME and CATEGORY."
  (gptel-make-tool
   :name name
   :function (lambda (&rest _) "")
   :description (format "Fake tool %s" name)
   :args nil
   :category (or category "niwa")))

(defun niwa-tools-list-test--cleanup-list (&rest buffers)
  "Kill tools cockpit test buffers and BUFFERS."
  (dolist (name (list niwa-tools-list-buffer-name
                      "*niwa tool details*"
                      niwa-tools-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun niwa-tools-list-test--open-list (session data-buffer &optional view-buffer)
  "Open a tools cockpit for SESSION owned by DATA-BUFFER."
  (let ((view-buffer (or view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (setq-local niwa--session session)
      (setq-local niwa--view-buffer view-buffer)
      (niwa-tools-list-open
       (list :view-buffer view-buffer
             :data-buffer data-buffer
             :origin-buffer data-buffer
             :session session
             :workspace (niwa-session-workspace session))))))


;;
;;; Tools listing surface

(niwa-deftest niwa-tools-list--status-cell ()
  ,test
  (test)

  :doc "formats state cells with visible labels"
  (dolist (state '(active discoverable))
    (should (equal (substring-no-properties
                    (niwa-tools-list--status-cell state))
                   (symbol-name state)))))

(niwa-deftest niwa-tools-list--item-id ()
  ,test
  (test)

  :doc "uses state, category, and name as the stable row id"
  (should (equal (niwa-tools-list--item-id
                  '(:state active :category "niwa" :name "Read"))
                 '(active "niwa" "Read"))))

(niwa-deftest niwa-tools-list--description-cell ()
  ,test
  (test)

  :doc "uses the first paragraph as a single-line table summary"
  (should (equal
           (niwa-tools-list--description-cell
            '(:description "First line\ncontinues here.\n\nFull details."))
           "First line continues here."))

  :doc "prefers an explicit summary"
  (should (equal
           (niwa-tools-list--description-cell
            '(:summary "Short summary" :description "Long details"))
           "Short summary")))

(niwa-deftest niwa-tools-list--tool-item ()
  ,test
  (test)

  :doc "builds active or pending items from gptel tools"
  (let* ((tool (niwa-tools-list-test--make-fake-gptel-tool "Read"))
         (item (niwa-tools-list--tool-item 'active tool)))
    (should (eq (plist-get item :state) 'active))
    (should (equal (plist-get item :name) "Read"))
    (should (equal (plist-get item :category) "niwa"))
    (should (equal (plist-get item :description) "Fake tool Read"))
    (should (eq (plist-get item :tool) tool))))

(niwa-deftest niwa-tools-list--catalog-item ()
  ,test
  (test)

  :doc "builds discoverable items from tool-catalog entries"
  (let ((item (niwa-tools-list--catalog-item
               '((niwa "Edit") . "Replace text"))))
    (should (eq (plist-get item :state) 'discoverable))
    (should (equal (plist-get item :name) "Edit"))
    (should (equal (plist-get item :category) 'niwa))
    (should (equal (plist-get item :description) "Replace text"))))



(niwa-deftest niwa-tools-list--collect-items
  (:after-each (niwa-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "collects active and discoverable items"
  (let* ((session (niwa-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-items*"))
         (active-tool (niwa-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (niwa-session-tool-catalog session)
                '((("niwa" "Imenu") . "List symbols")))
          (let ((items (niwa-tools-list--collect-items session data-buffer)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :state))
                                   items)
                           '(active discoverable)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :name))
                                   items)
                           '("Read" "Imenu")))))
      (niwa-tools-list-test--cleanup-list data-buffer))))

(niwa-deftest niwa-tools-list--entry ()
  ,test
  (test)

  :doc "builds table cells from tool item state"
  (let* ((item '(:state loaded :name "Imenu" :category "niwa"
                 :description "List symbols"))
         (entry (niwa-tools-list--entry item))
         (cells (niwa-test-tabulated-row-cells entry)))
    (should (equal (car entry) '(loaded "niwa" "Imenu")))
    (should (equal cells '("loaded" "Imenu" "niwa"
                           "List symbols"))))

  :doc "keeps multiline details out of the table cell"
  (let* ((item '(:state active :name "Agent" :category "niwa"
                 :description "Launch agents.\n\nForeground details."))
         (entry (niwa-tools-list--entry item))
         (cells (niwa-test-tabulated-row-cells entry)))
    (should (equal (nth 3 cells) "Launch agents."))))

(niwa-deftest niwa-tools-list--session-label ()
  ,test
  (test)

  :doc "returns the rendered session name or unknown"
  (let ((session (niwa-tools-list-test--make-session))
        (data-buffer (generate-new-buffer " *mt-tools-label*")))
    (with-temp-buffer
      (niwa-tools-list-mode)
      (should (equal (niwa-tools-list--session-label) "unknown")))
    (unwind-protect
        (let ((buffer (niwa-tools-list-test--open-list session data-buffer)))
          (with-current-buffer buffer
            (should (equal (niwa-tools-list--session-label
                            (niwa-cockpit-surface-context))
                           "main"))))
      (niwa-tools-list-test--cleanup-list data-buffer))))

(niwa-deftest niwa-tools-list--header-line ()
  ,test
  (test)

  :doc "summarizes row counts and points at the keys"
  (with-temp-buffer
    (niwa-tools-list-mode)
    (let ((line (niwa-tools-list--header-line
                 '((:state active :name "Read")
                   (:state discoverable :name "Edit")
                   (:state pending :name "Imenu")
                   (:state loaded :name "XrefReferences"))
                 nil)))
      (should (string-match-p "niwa: tools" line))
      (should (string-match-p "1 native" line))
      (should (string-match-p "1 discoverable" line))
      (should (string-match-p "? keys" line))
      ;; The key list belongs in `?' help, not in the header.
      (should-not (string-match-p "RET details" line)))))

(niwa-deftest niwa-tools-list-open
  (:after-each (progn
                 (niwa-tool-clear-registry)
                 (setf (alist-get "niwa" gptel--known-tools nil t #'equal)
                       nil)
                 (niwa-workspace-clear-registry)
                 (niwa-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "renders active and discoverable tool rows"
  (let* ((session (niwa-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-data*"))
         (active-tool (niwa-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (niwa-session-tool-catalog session)
                '((("niwa" "Edit") . "Replace text in a file")
                  (("niwa" "Imenu") . "List symbols in a file")))
          (let ((buffer (niwa-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (should (= 3 (length tabulated-list-entries)))
              (let ((rows (niwa-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc '(active "niwa" "Read")
                                           rows))
                               '("active" "Read" "niwa"
                                 "Fake tool Read")))
                (should (equal (cdr (assoc '(discoverable "niwa" "Edit")
                                           rows))
                               '("discoverable" "Edit" "niwa"
                                 "Replace text in a file")))
))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "rejects opening without a cockpit context"
  (with-temp-buffer
    (should-error (niwa-tools-list-open) :type 'user-error))

  :doc "distinguishes same-name rows by category"
  (let* ((session (niwa-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-state-category*")))
    (unwind-protect
        (progn
          (setf (niwa-session-tool-catalog session)
                '((("cat-a" "Edit") . "A")
                  (("cat-b" "Edit") . "B")))
          (let ((buffer (niwa-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id '(discoverable "cat-b" "Edit"))
              (let ((item (niwa-cockpit-surface-selected)))
                (should (eq (plist-get item :state) 'discoverable))
                (should (equal (plist-get item :category) "cat-b"))
                (should (equal (plist-get item :description) "B"))))))
      (niwa-tools-list-test--cleanup-list data-buffer))))

(niwa-deftest niwa-tools-list-refresh
  (:after-each (niwa-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "refresh updates visible discoverable row content"
  (let* ((session (niwa-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-refresh*"))
         (tool (niwa-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (setf (niwa-session-tool-catalog session)
                '((("niwa" "Edit") . "Replace text")))
          (let ((buffer (niwa-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id '(discoverable "niwa" "Edit"))
              (setcdr (car (niwa-session-tool-catalog session))
                      "Updated")
              (niwa-tools-list-refresh)
              (let ((rows (niwa-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc '(discoverable "niwa" "Edit")
                                           rows))
                               '("discoverable" "Edit" "niwa"
                                 "Updated")))))))
      (niwa-tools-list-test--cleanup-list data-buffer))))

(niwa-deftest niwa-tools-list--detail-report ()
  ,test
  (test)

  :doc "formats selected row details"
		 (let ((text (niwa-report-test-text (niwa-tools-list--detail-report
               '(:state loaded :name "Imenu" :category "niwa"
								 :description "List symbols\n\nFull guidance")))))
		   (should (string-match-p "Tool\nImenu \\[loaded\\]" text))
		   (should (string-match-p "Category +niwa" text))
    (should (string-match-p "Full guidance" text))))

(niwa-deftest niwa-tools-list-details
  (:after-each (niwa-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "opens details for the selected tool row"
  (let* ((session (niwa-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-details*"))
         (tool (niwa-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (let ((buffer (niwa-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (niwa-cockpit-goto-id '(active "niwa" "Read"))
              (niwa-tools-list-details))
            (with-current-buffer "*niwa tool details*"
			     (should (string-match-p "Tool\nRead \\[active\\]"
                                      (buffer-string))))))
      (niwa-tools-list-test--cleanup-list data-buffer))))









(niwa-deftest niwa-tools-list-open-gptel
  (:after-each (progn
                 (niwa-workspace-clear-registry)
                 (niwa-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "gptel bridge command runs from the paired data buffer"
  (let* ((session (niwa-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-gptel*"))
         called-buffer)
    (unwind-protect
        (progn
          (niwa-tools-list-test--open-list session data-buffer)
          (require 'gptel-transient)
          (with-current-buffer niwa-tools-list-buffer-name
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (niwa-tools-list-open-gptel)))
          (should (eq called-buffer data-buffer)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(niwa-deftest niwa-tools-list-help
  (:after-each (niwa-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "opens tools cockpit help"
  (progn
    (niwa-tools-list-help)
    (with-current-buffer niwa-tools-help-buffer-name
      (should (string-match-p "RET  Show selected tool details"
                              (buffer-string)))
      (should (string-match-p "s    Search tool contracts"
                              (buffer-string)))
      (should (string-match-p "g    Refresh table"
                              (buffer-string))))))

(niwa-deftest niwa-tools-list-quit
  (:after-each (niwa-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "kills a directly opened tools cockpit without opening the menu"
  (let* ((session (niwa-tools-list-test--make-session))
         (view-buffer (generate-new-buffer " *mt-tools-quit-view*"))
         (data-buffer (generate-new-buffer " *mt-tools-quit-data*"))
         called-buffer)
    (unwind-protect
        (let ((buffer (niwa-tools-list-test--open-list
                       session data-buffer view-buffer)))
          (require 'niwa-menu)
          (cl-letf (((symbol-function 'niwa-menu-open)
                     (lambda (&optional _area)
                       (setq called-buffer (current-buffer)))))
            (with-current-buffer buffer
              (niwa-tools-list-quit)))
          (should-not (buffer-live-p buffer))
          (should-not called-buffer))
      (niwa-tools-list-test--cleanup-list view-buffer data-buffer))))


(provide 'test-niwa-tools-list)
;;; test-niwa-tools-list.el ends here
