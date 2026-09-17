;;; test-mevedel-tools-list.el --- Tests for mevedel-tools-list.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'mevedel-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-report-test-support"))

(require 'gptel)
(require 'gptel-request)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'mevedel-tool-registry)
(require 'mevedel-agents)
(require 'mevedel-tools)
(require 'mevedel-tools-list)
(require 'mevedel-cockpit)
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

(defun mevedel-tools-list-test--make-session ()
  "Create a fresh tools-list-test session."
  (let ((ws (mevedel-workspace-get-or-create
             'project "/tmp/mt/" "/tmp/mt/" "mt")))
    (mevedel-session-create "main" ws)))

(defun mevedel-tools-list-test--make-fake-gptel-tool (name &optional category)
  "Return a minimal `gptel-tool' with NAME and CATEGORY."
  (gptel-make-tool
   :name name
   :function (lambda (&rest _) "")
   :description (format "Fake tool %s" name)
   :args nil
   :category (or category "mevedel")))

(defun mevedel-tools-list-test--cleanup-list (&rest buffers)
  "Kill tools cockpit test buffers and BUFFERS."
  (dolist (name (list mevedel-tools-list-buffer-name
                      "*mevedel tool details*"
                      mevedel-tools-help-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun mevedel-tools-list-test--open-list (session data-buffer &optional view-buffer)
  "Open a tools cockpit for SESSION owned by DATA-BUFFER."
  (let ((view-buffer (or view-buffer data-buffer)))
    (with-current-buffer data-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--view-buffer view-buffer)
      (mevedel-tools-list-open
       (list :view-buffer view-buffer
             :data-buffer data-buffer
             :origin-buffer data-buffer
             :session session
             :workspace (mevedel-session-workspace session))))))


;;
;;; Tools listing surface

(mevedel-deftest mevedel-tools-list--status-cell ()
  ,test
  (test)

  :doc "formats state cells with visible labels"
  (dolist (state '(active discoverable))
    (should (equal (substring-no-properties
                    (mevedel-tools-list--status-cell state))
                   (symbol-name state)))))

(mevedel-deftest mevedel-tools-list--item-id ()
  ,test
  (test)

  :doc "uses state, category, and name as the stable row id"
  (should (equal (mevedel-tools-list--item-id
                  '(:state active :category "mevedel" :name "Read"))
                 '(active "mevedel" "Read"))))

(mevedel-deftest mevedel-tools-list--description-cell ()
  ,test
  (test)

  :doc "uses the first paragraph as a single-line table summary"
  (should (equal
           (mevedel-tools-list--description-cell
            '(:description "First line\ncontinues here.\n\nFull details."))
           "First line continues here."))

  :doc "prefers an explicit summary"
  (should (equal
           (mevedel-tools-list--description-cell
            '(:summary "Short summary" :description "Long details"))
           "Short summary")))

(mevedel-deftest mevedel-tools-list--tool-item ()
  ,test
  (test)

  :doc "builds active or pending items from gptel tools"
  (let* ((tool (mevedel-tools-list-test--make-fake-gptel-tool "Read"))
         (item (mevedel-tools-list--tool-item 'active tool)))
    (should (eq (plist-get item :state) 'active))
    (should (equal (plist-get item :name) "Read"))
    (should (equal (plist-get item :category) "mevedel"))
    (should (equal (plist-get item :description) "Fake tool Read"))
    (should (eq (plist-get item :tool) tool))))

(mevedel-deftest mevedel-tools-list--catalog-item ()
  ,test
  (test)

  :doc "builds discoverable items from tool-catalog entries"
  (let ((item (mevedel-tools-list--catalog-item
               '((mevedel "Edit") . "Replace text"))))
    (should (eq (plist-get item :state) 'discoverable))
    (should (equal (plist-get item :name) "Edit"))
    (should (equal (plist-get item :category) 'mevedel))
    (should (equal (plist-get item :description) "Replace text"))))



(mevedel-deftest mevedel-tools-list--collect-items
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "collects active and discoverable items"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-items*"))
         (active-tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-tool-catalog session)
                '((("mevedel" "Imenu") . "List symbols")))
          (let ((items (mevedel-tools-list--collect-items session data-buffer)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :state))
                                   items)
                           '(active discoverable)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :name))
                                   items)
                           '("Read" "Imenu")))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--entry ()
  ,test
  (test)

  :doc "builds table cells from tool item state"
  (let* ((item '(:state loaded :name "Imenu" :category "mevedel"
                 :description "List symbols"))
         (entry (mevedel-tools-list--entry item))
         (cells (mevedel-test-tabulated-row-cells entry)))
    (should (equal (car entry) '(loaded "mevedel" "Imenu")))
    (should (equal cells '("loaded" "Imenu" "mevedel"
                           "List symbols"))))

  :doc "keeps multiline details out of the table cell"
  (let* ((item '(:state active :name "Agent" :category "mevedel"
                 :description "Launch agents.\n\nForeground details."))
         (entry (mevedel-tools-list--entry item))
         (cells (mevedel-test-tabulated-row-cells entry)))
    (should (equal (nth 3 cells) "Launch agents."))))

(mevedel-deftest mevedel-tools-list--session-label ()
  ,test
  (test)

  :doc "returns the rendered session name or unknown"
  (let ((session (mevedel-tools-list-test--make-session))
        (data-buffer (generate-new-buffer " *mt-tools-label*")))
    (with-temp-buffer
      (mevedel-tools-list-mode)
      (should (equal (mevedel-tools-list--session-label) "unknown")))
    (unwind-protect
        (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
          (with-current-buffer buffer
            (should (equal (mevedel-tools-list--session-label
                            (mevedel-cockpit-surface-context))
                           "main"))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--header-line ()
  ,test
  (test)

  :doc "summarizes row counts and points at the keys"
  (with-temp-buffer
    (mevedel-tools-list-mode)
    (let ((line (mevedel-tools-list--header-line
                 '((:state active :name "Read")
                   (:state discoverable :name "Edit")
                   (:state pending :name "Imenu")
                   (:state loaded :name "XrefReferences"))
                 nil)))
      (should (string-match-p "mevedel: tools" line))
      (should (string-match-p "1 native" line))
      (should (string-match-p "1 discoverable" line))
      (should (string-match-p "? keys" line))
      ;; The key list belongs in `?' help, not in the header.
      (should-not (string-match-p "RET details" line)))))

(mevedel-deftest mevedel-tools-list-open
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "renders active and discoverable tool rows"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-data*"))
         (active-tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-tool-catalog session)
                '((("mevedel" "Edit") . "Replace text in a file")
                  (("mevedel" "Imenu") . "List symbols in a file")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (should (= 3 (length tabulated-list-entries)))
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc '(active "mevedel" "Read")
                                           rows))
                               '("active" "Read" "mevedel"
                                 "Fake tool Read")))
                (should (equal (cdr (assoc '(discoverable "mevedel" "Edit")
                                           rows))
                               '("discoverable" "Edit" "mevedel"
                                 "Replace text in a file")))
))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "rejects opening without a cockpit context"
  (with-temp-buffer
    (should-error (mevedel-tools-list-open) :type 'user-error))

  :doc "distinguishes same-name rows by category"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-state-category*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-tool-catalog session)
                '((("cat-a" "Edit") . "A")
                  (("cat-b" "Edit") . "B")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(discoverable "cat-b" "Edit"))
              (let ((item (mevedel-cockpit-surface-selected)))
                (should (eq (plist-get item :state) 'discoverable))
                (should (equal (plist-get item :category) "cat-b"))
                (should (equal (plist-get item :description) "B"))))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list-refresh
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "refresh updates visible discoverable row content"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-refresh*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-tool-catalog session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(discoverable "mevedel" "Edit"))
              (setcdr (car (mevedel-session-tool-catalog session))
                      "Updated")
              (mevedel-tools-list-refresh)
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc '(discoverable "mevedel" "Edit")
                                           rows))
                               '("discoverable" "Edit" "mevedel"
                                 "Updated")))))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--detail-report ()
  ,test
  (test)

  :doc "formats selected row details"
		 (let ((text (mevedel-report-test-text (mevedel-tools-list--detail-report
               '(:state loaded :name "Imenu" :category "mevedel"
								 :description "List symbols\n\nFull guidance")))))
		   (should (string-match-p "Tool\nImenu \\[loaded\\]" text))
		   (should (string-match-p "Category +mevedel" text))
    (should (string-match-p "Full guidance" text))))

(mevedel-deftest mevedel-tools-list-details
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "opens details for the selected tool row"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-details*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (mevedel-tools-list-details))
            (with-current-buffer "*mevedel tool details*"
			     (should (string-match-p "Tool\nRead \\[active\\]"
                                      (buffer-string))))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))









(mevedel-deftest mevedel-tools-list-open-gptel
  (:after-each (progn
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "gptel bridge command runs from the paired data buffer"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-gptel*"))
         called-buffer)
    (unwind-protect
        (progn
          (mevedel-tools-list-test--open-list session data-buffer)
          (require 'gptel-transient)
          (with-current-buffer mevedel-tools-list-buffer-name
            (cl-letf (((symbol-function 'gptel-menu)
                       (lambda ()
                         (interactive)
                         (setq called-buffer (current-buffer)))))
              (mevedel-tools-list-open-gptel)))
          (should (eq called-buffer data-buffer)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-tools-list-help
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "opens tools cockpit help"
  (progn
    (mevedel-tools-list-help)
    (with-current-buffer mevedel-tools-help-buffer-name
      (should (string-match-p "RET  Show selected tool details"
                              (buffer-string)))
      (should (string-match-p "s    Search tool contracts"
                              (buffer-string)))
      (should (string-match-p "g    Refresh table"
                              (buffer-string))))))

(mevedel-deftest mevedel-tools-list-quit
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "kills the tools cockpit and reopens the main session cockpit"
  (let* ((session (mevedel-tools-list-test--make-session))
         (view-buffer (generate-new-buffer " *mt-tools-quit-view*"))
         (data-buffer (generate-new-buffer " *mt-tools-quit-data*"))
         called-buffer)
    (unwind-protect
        (let ((buffer (mevedel-tools-list-test--open-list
                       session data-buffer view-buffer)))
          (require 'mevedel-menu)
          (cl-letf (((symbol-function 'mevedel-menu-open)
                     (lambda (&optional _area)
                       (setq called-buffer (current-buffer)))))
            (with-current-buffer buffer
              (mevedel-tools-list-quit)))
          (should-not (buffer-live-p buffer))
          (should (eq called-buffer data-buffer)))
      (mevedel-tools-list-test--cleanup-list view-buffer data-buffer))))


(provide 'test-mevedel-tools-list)
;;; test-mevedel-tools-list.el ends here
