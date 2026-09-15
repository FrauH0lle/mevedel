;;; mevedel-tools-list.el -- Tools cockpit list -*- lexical-binding: t -*-

;;; Commentary:

;; Tools cockpit surface for inspecting native tools and specialist contracts.
;; The underlying discovery mechanics live in `mevedel-tools';
;; this module owns only the tabulated UI.

;;; Code:

(require 'mevedel-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

(require 'subr-x)
(require 'mevedel-cockpit)
(require 'mevedel-structs)

;; `gptel'
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-description "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)

;; `gptel-transient'
(declare-function gptel-menu "ext:gptel-transient" ())

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-format-header
                  "mevedel-cockpit" (name scope state))
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-show-help
                  "mevedel-cockpit" (buffer text))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-details
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-key-help-text
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-structs'
(declare-function mevedel-session-tool-catalog
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--session)

;; `mevedel-tools'
(declare-function mevedel-tools--tool-search
                  "mevedel-tools" (callback query))

;; `mevedel-utilities'
(declare-function mevedel--truncate-display
                  "mevedel-utilities" (text width &optional ellipsis))
(autoload 'mevedel--truncate-display "mevedel-utilities")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())


;;
;;; Tools listing surface

(defconst mevedel-tools-list-buffer-name "*mevedel tools*"
  "Name of the tools listing buffer.")

(defconst mevedel-tools-help-buffer-name "*mevedel tools help*"
  "Name of the tools cockpit help buffer.")

(defun mevedel-tools-list--status-cell (state)
  "Return the propertized table status cell for STATE."
  (let ((label (symbol-name state)))
    (propertize
     label 'face
     (pcase state
       ('active 'success)
       ('discoverable 'shadow)
       (_ 'default)))))

(defun mevedel-tools-list--item-id (item)
  "Return stable tabulated row id for ITEM."
  (list (plist-get item :state)
        (format "%s" (or (plist-get item :category) ""))
        (plist-get item :name)))

(defun mevedel-tools-list--tool-matches-item-p (tool item)
  "Return non-nil when TOOL is represented by ITEM."
  (and (equal (gptel-tool-name tool) (plist-get item :name))
       (equal (format "%s" (or (gptel-tool-category tool) ""))
              (format "%s" (or (plist-get item :category) "")))))

(defun mevedel-tools-list--description-cell (item)
  "Return the compact table description for ITEM."
  (let* ((text (or (plist-get item :summary)
                   (plist-get item :description)
                   ""))
         (paragraph (car (split-string text "\n[ \t]*\n" t)))
         (one-line (replace-regexp-in-string
                    "[ \t\n\r]+" " " (or paragraph ""))))
    (mevedel--truncate-display (string-trim one-line) 96 "...")))

(defun mevedel-tools-list--tool-item (state tool)
  "Return a tools cockpit item for TOOL in STATE."
  (let ((description (gptel-tool-description tool)))
    (list :state state
          :name (gptel-tool-name tool)
          :category (or (gptel-tool-category tool) "")
          :description (if (stringp description) description "")
          :tool tool)))

(defun mevedel-tools-list--catalog-item (entry)
  "Return a tools cockpit item for discoverable ENTRY."
  (pcase-let ((`((,category ,name) . ,summary) entry))
    (list :state 'discoverable
          :name name
          :category (or category "")
          :description (if (stringp summary) summary "")
          :entry entry)))

(defun mevedel-tools-list--collect-items (session data-buffer)
  "Return native and discoverable rows for SESSION and DATA-BUFFER."
  (let ((active (and (buffer-live-p data-buffer)
                     (buffer-local-value 'gptel-tools data-buffer)))
        (catalog (and session (mevedel-session-tool-catalog session))))
    (append (mapcar (lambda (tool) (mevedel-tools-list--tool-item 'active tool)) active)
            (mapcar #'mevedel-tools-list--catalog-item catalog))))

(defun mevedel-tools-list--entry (item &optional _context)
  "Return a `tabulated-list-mode' row for ITEM."
  (list
   (mevedel-tools-list--item-id item)
   (vector
    (mevedel-tools-list--status-cell (plist-get item :state))
    (plist-get item :name)
    (format "%s" (or (plist-get item :category) ""))
    (mevedel-tools-list--description-cell item))))

(defun mevedel-tools-list--session-label (&optional context)
  "Return CONTEXT's tools cockpit session label."
  (if-let* ((session (and context
                          (mevedel-cockpit-context-session context))))
      (mevedel-session-name session)
    "unknown"))

(defun mevedel-tools-list--header-line (&optional items context)
  "Return the tools cockpit header line for ITEMS and CONTEXT."
  (let ((counts nil))
    (dolist (item items)
      (cl-incf (alist-get (plist-get item :state) counts 0)))
    (mevedel-cockpit-format-header
     "tools"
     (mevedel-tools-list--session-label context)
     (format "%d native · %d discoverable"
             (alist-get 'active counts 0)
             (alist-get 'discoverable counts 0)))))

(defun mevedel-tools-list--context ()
  "Return the current tools cockpit context."
  (mevedel-cockpit-surface-context))

(defun mevedel-tools-list--context-data-buffer ()
  "Return the current tools cockpit data buffer."
  (mevedel-cockpit-context-data-buffer (mevedel-tools-list--context)))

(defun mevedel-tools-list--collect (context)
  "Return tools cockpit items for CONTEXT."
  (let ((session (or (mevedel-cockpit-context-session context)
                     (user-error "No mevedel session in this buffer")))
        (data-buffer (mevedel-cockpit-context-data-buffer context)))
    (mevedel-tools-list--collect-items session data-buffer)))

(defun mevedel-tools-list-refresh ()
  "Refresh the current tools listing buffer."
  (interactive)
  (mevedel-cockpit-surface-refresh))

(defun mevedel-tools-list--selected-item ()
  "Return the selected tools cockpit item, or nil."
  (mevedel-cockpit-surface-selected t))

(defun mevedel-tools-list--selected-item-for-state (state)
  "Return the selected tools cockpit item when its state is STATE."
  (condition-case nil
      (when-let* ((item (mevedel-tools-list--selected-item)))
        (and (eq (plist-get item :state) state)
             item))
    (user-error nil)))

(defun mevedel-tools-list--detail-report (item &optional _context)
  "Return the information report for tool ITEM."
  (list :title "Tool" :subtitle (format "%s [%s]" (plist-get item :name) (plist-get item :state))
        :identity (plist-get item :name)
        :sections
        (list
         (list :id 'tool :title "Tool"
               :body (mevedel-report-fields
                      (list "Name" (plist-get item :name))
                      (list "State" (plist-get item :state))
                      (list "Category" (or (plist-get item :category) ""))))
         (list :id 'description :title "Description" :mode 'markdown-mode
               :body (or (plist-get item :description) "")))))

(defun mevedel-tools-list-details ()
  "Show details for the tool row at point."
  (interactive)
  (mevedel-cockpit-surface-details))

(defun mevedel-tools-list-search (&optional query)
  "Retrieve discoverable tool contracts matching QUERY."
  (interactive)
  (let* ((context (mevedel-tools-list--context))
         (session (or (mevedel-cockpit-context-session context)
                      (user-error "No mevedel session in this buffer")))
         (data-buffer (mevedel-tools-list--context-data-buffer))
         (candidates (delete-dups
                      (mapcar #'cadar
                              (mevedel-session-tool-catalog session))))
         (query (or query
                    (completing-read
                     "Search tool contracts: "
                     candidates nil nil nil nil (car candidates))))
         result)
    (when (string-empty-p (string-trim query))
      (user-error "Search query cannot be empty"))
    (with-current-buffer data-buffer
      (let ((mevedel--agent-invocation nil)
            (mevedel--session session))
        (mevedel-tools--tool-search
         (lambda (text) (setq result text))
         query)))
    (mevedel-tools-list-refresh)
    (message "%s" result)
    result))

(defun mevedel-tools-list-open-gptel ()
  "Open gptel-menu from the tools listing's data buffer."
  (interactive)
  (require 'gptel-transient)
  (let ((data-buffer (mevedel-tools-list--context-data-buffer)))
    (with-current-buffer data-buffer
      (call-interactively #'gptel-menu))))

(defun mevedel-tools-list--help-report (&optional _context)
  "Return the complete cockpit help report."
  (list :title "Tools help"
        :sections (list (list :id 'keys :title "Keys"
                              :body (mevedel-cockpit-surface-key-help-text mevedel-tools-list--surface))
			(list :id 'rows :title "Rows" :body "active    Native tools in the current payload\ndiscoverable  Contracts through ToolSearch; calls through ToolCall"))))

(defun mevedel-tools-list-help ()
  "Open tools cockpit help."
  (interactive)
  (mevedel-cockpit-show-help
   mevedel-tools-help-buffer-name
   (mevedel-tools-list--help-report)))

(defun mevedel-tools-list-quit ()
  "Quit the tools cockpit and return to the main session cockpit."
  (interactive)
  (mevedel-cockpit-quit "tools cockpit"))

(defconst mevedel-tools-list--surface
  `(:buffer-name ,mevedel-tools-list-buffer-name
    :label "tools cockpit"
    :row-label "tool"
    :mode mevedel-tools-list-mode
    :format [("State" 10 t)
             ("Name" 24 t)
             ("Category" 16 t)
             ("Description" 0 t)]
    :sort-key ("Name" . nil)
    :require-session t
    :collect mevedel-tools-list--collect
    :entry mevedel-tools-list--entry
    :header mevedel-tools-list--header-line
    :details mevedel-tools-list--detail-report
    :details-buffer "*mevedel tool details*"
    :help-buffer ,mevedel-tools-help-buffer-name
    :help-function mevedel-tools-list--help-report
    :keys (("s" "Search tool contracts"
            mevedel-tools-list-search)
           ("G" "Open gptel menu from the owning data buffer"
            mevedel-tools-list-open-gptel)))
  "Cockpit surface spec for the tools list.")

(define-derived-mode mevedel-tools-list-mode tabulated-list-mode
  "mevedel-tools"
  "Major mode for managing mevedel tool state."
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-tools-list--surface))

(defun mevedel-tools-list-open (&optional context)
  "Open the tools listing buffer for CONTEXT."
  (require 'mevedel-tools)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (unless (mevedel-cockpit-context-session context)
      (user-error "No mevedel session in this buffer"))
    (mevedel-cockpit-open-surface mevedel-tools-list--surface context)))

(provide 'mevedel-tools-list)
;;; mevedel-tools-list.el ends here
