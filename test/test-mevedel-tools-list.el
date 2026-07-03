;;; test-mevedel-tools-list.el --- Tests for mevedel-tools-list.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
  (dolist (state '(active deferred pending loaded expired))
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

(mevedel-deftest mevedel-tools-list--tool-matches-item-p ()
  ,test
  (test)

  :doc "matches tools by both category and name"
  (let ((tool (mevedel-tools-list-test--make-fake-gptel-tool "Run" "cat-a")))
    (should (mevedel-tools-list--tool-matches-item-p
             tool '(:category "cat-a" :name "Run")))
    (should-not (mevedel-tools-list--tool-matches-item-p
                 tool '(:category "cat-b" :name "Run")))
    (should-not (mevedel-tools-list--tool-matches-item-p
                 tool '(:category "cat-a" :name "Read")))))

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

(mevedel-deftest mevedel-tools-list--deferred-item ()
  ,test
  (test)

  :doc "builds deferred items from deferred-set entries"
  (let ((item (mevedel-tools-list--deferred-item
               '((mevedel "Edit") . "Replace text"))))
    (should (eq (plist-get item :state) 'deferred))
    (should (equal (plist-get item :name) "Edit"))
    (should (equal (plist-get item :category) 'mevedel))
    (should (equal (plist-get item :description) "Replace text"))))

(mevedel-deftest mevedel-tools-list--loaded-item ()
  ,test
  (test)

  :doc "builds loaded items with ttl cells"
  (let ((item (mevedel-tools-list--loaded-item '("Imenu" . 3))))
    (should (eq (plist-get item :state) 'loaded))
    (should (equal (plist-get item :name) "Imenu"))
    (should (equal (plist-get item :ttl) "3"))
    (should (string-match-p "Temporarily loaded"
                            (plist-get item :description)))))

(mevedel-deftest mevedel-tools-list--expired-item ()
  ,test
  (test)

  :doc "builds expired items"
  (let ((item (mevedel-tools-list--expired-item "Treesitter")))
    (should (eq (plist-get item :state) 'expired))
    (should (equal (plist-get item :name) "Treesitter"))
    (should (string-match-p "Expired" (plist-get item :description)))))

(mevedel-deftest mevedel-tools-list--collect-items
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "collects active, deferred, pending, loaded, and expired items"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-items*"))
         (active-tool (mevedel-tools-list-test--make-fake-gptel-tool "Read"))
         (pending-tool (mevedel-tools-list-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Imenu") . "List symbols")))
          (setf (mevedel-session-deferred-pending session)
                (list pending-tool))
          (setf (mevedel-session-deferred-injected session)
                '(("XrefReferences" . 2)))
          (setf (mevedel-session-deferred-expired session)
                '("Treesitter"))
          (let ((items (mevedel-tools-list--collect-items session data-buffer)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :state))
                                   items)
                           '(active deferred pending loaded expired)))
            (should (equal (mapcar (lambda (item)
                                     (plist-get item :name))
                                   items)
                           '("Read" "Imenu" "Edit"
                             "XrefReferences" "Treesitter")))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--entry ()
  ,test
  (test)

  :doc "builds table cells from tool item state"
  (let* ((item '(:state loaded :name "Imenu" :category "mevedel"
                 :ttl "3" :description "List symbols"))
         (entry (mevedel-tools-list--entry item))
         (cells (mevedel-test-tabulated-row-cells entry)))
    (should (equal (car entry) '(loaded "mevedel" "Imenu")))
    (should (equal cells '("loaded" "Imenu" "mevedel" "3"
                           "List symbols"))))

  :doc "keeps multiline details out of the table cell"
  (let* ((item '(:state active :name "Agent" :category "mevedel"
                 :description "Launch agents.\n\nForeground details."))
         (entry (mevedel-tools-list--entry item))
         (cells (mevedel-test-tabulated-row-cells entry)))
    (should (equal (nth 4 cells) "Launch agents."))))

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

  :doc "summarizes row counts and key hints"
  (with-temp-buffer
    (mevedel-tools-list-mode)
    (let ((line (mevedel-tools-list--header-line
                 '((:state active :name "Read")
                   (:state deferred :name "Edit")
                   (:state pending :name "Imenu")
                   (:state loaded :name "XrefReferences")
                   (:state expired :name "Treesitter"))
                 nil)))
      (should (string-match-p
               (format "default TTL:%d" mevedel-deferred-tool-ttl)
               line))
      (should (string-match-p "active:1" line))
      (should (string-match-p "deferred:1" line))
      (should (string-match-p "RET details" line))
      (should (string-match-p "q back" line)))))

(mevedel-deftest mevedel-tools-list-open
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "renders active, deferred, pending, loaded, and expired tool rows"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-data*"))
         (active-tool (mevedel-tools-list-test--make-fake-gptel-tool "Read"))
         (pending-tool (mevedel-tools-list-test--make-fake-gptel-tool "Edit")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text in a file")
                  (("mevedel" "Imenu") . "List symbols in a file")))
          (setf (mevedel-session-deferred-pending session)
                (list pending-tool))
          (setf (mevedel-session-deferred-injected session)
                '(("XrefReferences" . 3)))
          (setf (mevedel-session-deferred-expired session)
                '("Treesitter"))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (should (= 6 (length tabulated-list-entries)))
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc '(active "mevedel" "Read")
                                           rows))
                               '("active" "Read" "mevedel" ""
                                 "Fake tool Read")))
                (should (equal (cdr (assoc '(deferred "mevedel" "Edit")
                                           rows))
                               '("deferred" "Edit" "mevedel" ""
                                 "Replace text in a file")))
                (should (equal (cdr (assoc '(pending "mevedel" "Edit")
                                           rows))
                               '("pending" "Edit" "mevedel" ""
                                 "Fake tool Edit")))
                (should (equal (cdr (assoc '(loaded "" "XrefReferences")
                                           rows))
                               '("loaded" "XrefReferences" "" "3"
                                 "Temporarily loaded deferred tool")))
                (should (equal (cdr (assoc '(expired "" "Treesitter")
                                           rows))
                               '("expired" "Treesitter" "" ""
                                 "Expired after its deferred-tool TTL elapsed")))))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "rejects opening without a cockpit context"
  (with-temp-buffer
    (should-error (mevedel-tools-list-open) :type 'user-error)))

(mevedel-deftest mevedel-tools-list-refresh
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "refresh updates visible deferred row content"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-refresh*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "mevedel" "Edit"))
              (setcdr (car (mevedel-session-deferred-set session))
                      "Updated")
              (mevedel-tools-list-refresh)
              (let ((rows (mevedel-test-tabulated-entries-cells)))
                (should (equal (cdr (assoc '(deferred "mevedel" "Edit")
                                           rows))
                               '("deferred" "Edit" "mevedel" ""
                                 "Updated")))))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--selected-item-for-state
  (:after-each (mevedel-tools-list-test--cleanup-list))
  ,test
  (test)

  :doc "returns selected row item only for the requested state"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-state-name*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (let ((item (mevedel-tools-list--selected-item-for-state
                           'active)))
                (should (equal (plist-get item :name) "Read")))
              (should-not (mevedel-tools-list--selected-item-for-state
                           'deferred)))))
      (mevedel-tools-list-test--cleanup-list data-buffer)))

  :doc "distinguishes same-name rows by category"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-state-category*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-deferred-set session)
                '((("cat-a" "Edit") . "A")
                  (("cat-b" "Edit") . "B")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "cat-b" "Edit"))
              (let ((item (mevedel-tools-list--selected-item-for-state
                           'deferred)))
                (should (equal (plist-get item :category) "cat-b"))
                (should (equal (plist-get item :description) "B"))))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--detail-text ()
  ,test
  (test)

  :doc "formats selected row details"
  (let ((text (mevedel-tools-list--detail-text
               '(:state loaded :name "Imenu" :category "mevedel"
                 :ttl "3" :description "List symbols\n\nFull guidance"))))
    (should (string-match-p "Tool Imenu \\[loaded\\]" text))
    (should (string-match-p "Category: mevedel" text))
    (should (string-match-p "TTL: 3" text))
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
              (should (string-match-p "Tool Read \\[active\\]"
                                      (buffer-string))))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list--main-data-buffer
  (:after-each (progn
                 (mevedel-workspace-clear-registry)
                 (setq mevedel-agent--registry nil)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "rejects agent data buffers before lifecycle mutation"
  (let* ((_ (mevedel-define-agent tool-child :description "child" :tools nil))
         (agent (mevedel-agent-get "tool-child"))
         (inv (mevedel-agent-invocation-create agent))
         (session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-child*")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local mevedel--agent-invocation inv))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (should-error (mevedel-tools-list--main-data-buffer)
                            :type 'user-error))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

(mevedel-deftest mevedel-tools-list-defer-active
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "moves selected active tool into only the current session's deferred set"
  (let* ((session (mevedel-tools-list-test--make-session))
         (other-session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-defer*"))
         (other-buffer (generate-new-buffer " *mt-tools-other*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Read"))
         (known-before (copy-tree gptel--known-tools)))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (with-current-buffer other-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-deferred-set other-session)
                '((("mevedel" "Keep") . "keep")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (mevedel-tools-list-defer-active)
              (should (assoc '(deferred "mevedel" "Read")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (null gptel-tools)))
          (with-current-buffer other-buffer
            (should (equal (list tool) gptel-tools)))
          (should (equal "Read" (cadr (caar (mevedel-session-deferred-set
                                             session)))))
          (should (equal '((("mevedel" "Keep") . "keep"))
                         (mevedel-session-deferred-set other-session)))
          (should (equal known-before gptel--known-tools)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (when (buffer-live-p other-buffer)
        (kill-buffer other-buffer))))

  :doc "falls back to the active-tool prompt when point is not active"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-defer-prompt*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "mevedel" "Edit"))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Read")))
                (mevedel-tools-list-defer-active))
              (should (assoc '(deferred "mevedel" "Read")
                             tabulated-list-entries)))))
      (mevedel-tools-list-test--cleanup-list data-buffer)))

  :doc "selected active row only defers the matching category"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-defer-category*"))
         (cat-a (mevedel-tools-list-test--make-fake-gptel-tool "Run" "cat-a"))
         (cat-b (mevedel-tools-list-test--make-fake-gptel-tool "Run" "cat-b")))
    (unwind-protect
        (progn
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list cat-a cat-b)))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "cat-b" "Run"))
              (mevedel-tools-list-defer-active)
              (should (assoc '(deferred "cat-b" "Run")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (equal (list cat-a) gptel-tools)))
          (should (equal '((("cat-b" "Run") . "Fake tool Run"))
                         (mevedel-session-deferred-set session))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list-activate-deferred
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (setf (alist-get "cat-a" gptel--known-tools nil t #'equal)
                       nil)
                 (setf (alist-get "cat-b" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "moves selected deferred tool into only the current session's active tools"
  (let* ((session (mevedel-tools-list-test--make-session))
         (other-session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-activate*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Edit"))
         known-before)
    (unwind-protect
        (progn
          (setf (alist-get "Edit"
                           (alist-get "mevedel"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                tool)
          (setq known-before (copy-tree gptel--known-tools))
          (with-current-buffer data-buffer
            (setq-local gptel-tools nil))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text in a file")))
          (setf (mevedel-session-deferred-injected session) '(("Edit" . 2)))
          (setf (mevedel-session-deferred-expired session) '("Edit"))
          (setf (mevedel-session-deferred-set other-session)
                '((("mevedel" "Edit") . "Other session copy")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "mevedel" "Edit"))
              (mevedel-tools-list-activate-deferred)
              (should (assoc '(active "mevedel" "Edit")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (equal "Edit" (gptel-tool-name (car gptel-tools)))))
          (should (null (mevedel-session-deferred-set session)))
          (should (null (mevedel-session-deferred-injected session)))
          (should (null (mevedel-session-deferred-expired session)))
          (should (equal '((("mevedel" "Edit") . "Other session copy"))
                         (mevedel-session-deferred-set other-session)))
          (should (equal known-before gptel--known-tools)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))))

  :doc "falls back to the deferred-tool prompt when point is not deferred"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-activate-prompt*"))
         (tool (mevedel-tools-list-test--make-fake-gptel-tool "Edit"))
         (active-tool (mevedel-tools-list-test--make-fake-gptel-tool "Read")))
    (unwind-protect
        (progn
          (setf (alist-get "Edit"
                           (alist-get "mevedel"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                tool)
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list active-tool)))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(active "mevedel" "Read"))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "Edit")))
                (mevedel-tools-list-activate-deferred))
              (should (assoc '(active "mevedel" "Edit")
                             tabulated-list-entries)))))
      (mevedel-tools-list-test--cleanup-list data-buffer)))

  :doc "selected deferred row only activates the matching category"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-activate-category*"))
         (cat-a (mevedel-tools-list-test--make-fake-gptel-tool "Run" "cat-a"))
         (cat-b (mevedel-tools-list-test--make-fake-gptel-tool "Run" "cat-b")))
    (unwind-protect
        (progn
          (setf (alist-get "Run"
                           (alist-get "cat-a"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                cat-a)
          (setf (alist-get "Run"
                           (alist-get "cat-b"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                cat-b)
          (setf (mevedel-session-deferred-set session)
                '((("cat-a" "Run") . "A")
                  (("cat-b" "Run") . "B")))
          (with-current-buffer data-buffer
            (setq-local gptel-tools (list cat-a)))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (mevedel-cockpit-goto-id '(deferred "cat-b" "Run"))
              (mevedel-tools-list-activate-deferred)
              (should (assoc '(active "cat-b" "Run")
                             tabulated-list-entries))))
          (with-current-buffer data-buffer
            (should (equal (list cat-b cat-a) gptel-tools)))
          (should (equal '((("cat-a" "Run") . "A"))
                         (mevedel-session-deferred-set session))))
      (mevedel-tools-list-test--cleanup-list data-buffer))))

(mevedel-deftest mevedel-tools-list-search-load
  (:after-each (progn
                 (mevedel-tool-clear-registry)
                 (setf (alist-get "mevedel" gptel--known-tools nil t #'equal)
                       nil)
                 (mevedel-workspace-clear-registry)
                 (mevedel-tools-list-test--cleanup-list)))
  ,test
  (test)

  :doc "search/load queues matching deferred tools and refreshes pending state"
  (let* ((session (mevedel-tools-list-test--make-session))
         (data-buffer (generate-new-buffer " *mt-tools-search*"))
         (gtool (mevedel-tools-list-test--make-fake-gptel-tool "Edit"))
         (tool (mevedel-tool--create
                :name "Edit" :category "mevedel"
                :gptel-tool gtool))
         known-before)
    (unwind-protect
        (progn
          (mevedel-tool-register tool)
          (setf (alist-get "Edit"
                           (alist-get "mevedel"
                                      gptel--known-tools nil nil #'equal)
                           nil nil #'equal)
                gtool)
          (setq known-before (copy-tree gptel--known-tools))
          (with-current-buffer data-buffer
            (setq-local gptel-tools nil))
          (setf (mevedel-session-deferred-set session)
                '((("mevedel" "Edit") . "Replace text in a file")))
          (let ((buffer (mevedel-tools-list-test--open-list session data-buffer)))
            (with-current-buffer buffer
              (let ((result (mevedel-tools-list-search-load "edit")))
                (should (string-match-p "available now" result)))
              (should (= 1 (length (mevedel-session-deferred-pending
                                    session))))
              (should (assoc '(pending "mevedel" "Edit")
                             tabulated-list-entries))
              (should (equal known-before gptel--known-tools)))))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer)))))

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
      (should (string-match-p "l    Search and load"
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
