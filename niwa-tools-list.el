;;; niwa-tools-list.el -- Tools cockpit list -*- lexical-binding: t -*-

;;; Commentary:

;; Tools cockpit surface for inspecting native tools and specialist contracts.
;; The underlying discovery mechanics live in `niwa-tools';
;; this module owns only the tabulated UI.

;;; Code:

(require 'niwa-report)

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

(require 'subr-x)
(require 'niwa-cockpit)
(require 'niwa-structs)

(defvar niwa-tools-list--surface)

;; `gptel'
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-description "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)

;; `gptel-transient'
(declare-function gptel-menu "ext:gptel-transient" ())

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-data-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-show-help
                  "niwa-cockpit" (buffer text))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-details
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-surface-key-help-text
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))

;; `niwa-structs'
(declare-function niwa-session-tool-catalog
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(defvar niwa--agent-invocation)
(defvar niwa--session)

;; `niwa-tools'
(declare-function niwa-tools--tool-search
                  "niwa-tools" (callback query))

;; `niwa-utilities'
(declare-function niwa--truncate-display
                  "niwa-utilities" (text width &optional ellipsis))
(autoload 'niwa--truncate-display "niwa-utilities")

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())


;;
;;; Tools listing surface

(defconst niwa-tools-list-buffer-name "*niwa tools*"
  "Name of the tools listing buffer.")

(defconst niwa-tools-help-buffer-name "*niwa tools help*"
  "Name of the tools cockpit help buffer.")

(defun niwa-tools-list--status-cell (state)
  "Return the propertized table status cell for STATE."
  (let ((label (symbol-name state)))
    (propertize
     label 'face
     (pcase state
       ('active 'success)
       ('discoverable 'shadow)
       (_ 'default)))))

(defun niwa-tools-list--item-id (item)
  "Return stable tabulated row id for ITEM."
  (list (plist-get item :state)
        (format "%s" (or (plist-get item :category) ""))
        (plist-get item :name)))

(defun niwa-tools-list--description-cell (item)
  "Return the compact table description for ITEM."
  (let* ((text (or (plist-get item :summary)
                   (plist-get item :description)
                   ""))
         (paragraph (car (split-string text "\n[ \t]*\n" t)))
         (one-line (replace-regexp-in-string
                    "[ \t\n\r]+" " " (or paragraph ""))))
    (niwa--truncate-display (string-trim one-line) 96 "...")))

(defun niwa-tools-list--tool-item (state tool)
  "Return a tools cockpit item for TOOL in STATE."
  (let ((description (gptel-tool-description tool)))
    (list :state state
          :name (gptel-tool-name tool)
          :category (or (gptel-tool-category tool) "")
          :description (if (stringp description) description "")
          :tool tool)))

(defun niwa-tools-list--catalog-item (entry)
  "Return a tools cockpit item for discoverable ENTRY."
  (pcase-let ((`((,category ,name) . ,summary) entry))
    (list :state 'discoverable
          :name name
          :category (or category "")
          :description (if (stringp summary) summary "")
          :entry entry)))

(defun niwa-tools-list--collect-items (session data-buffer)
  "Return native and discoverable rows for SESSION and DATA-BUFFER."
  (let ((active (and (buffer-live-p data-buffer)
                     (buffer-local-value 'gptel-tools data-buffer)))
        (catalog (and session (niwa-session-tool-catalog session))))
    (append (mapcar (lambda (tool) (niwa-tools-list--tool-item 'active tool)) active)
            (mapcar #'niwa-tools-list--catalog-item catalog))))

(defun niwa-tools-list--entry (item &optional _context)
  "Return a `tabulated-list-mode' row for ITEM."
  (list
   (niwa-tools-list--item-id item)
   (vector
    (niwa-tools-list--status-cell (plist-get item :state))
    (plist-get item :name)
    (format "%s" (or (plist-get item :category) ""))
    (niwa-tools-list--description-cell item))))

(defun niwa-tools-list--session-label (&optional context)
  "Return CONTEXT's tools cockpit session label."
  (if-let* ((session (and context
                          (niwa-cockpit-context-session context))))
      (niwa-session-name session)
    "unknown"))

(defun niwa-tools-list--header-line (&optional items context)
  "Return the tools cockpit header line for ITEMS and CONTEXT."
  (let ((counts nil))
    (dolist (item items)
      (cl-incf (alist-get (plist-get item :state) counts 0)))
    (niwa-cockpit-format-header
     "tools"
     (niwa-tools-list--session-label context)
     (format "%d native · %d discoverable"
             (alist-get 'active counts 0)
             (alist-get 'discoverable counts 0)))))

(defun niwa-tools-list--context-data-buffer ()
  "Return the current tools cockpit data buffer."
  (niwa-cockpit-context-data-buffer (niwa-cockpit-surface-context)))

(defun niwa-tools-list--collect (context)
  "Return tools cockpit items for CONTEXT."
  (let ((session (or (niwa-cockpit-context-session context)
                     (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                                  "No niwa session in this buffer")))
        (data-buffer (niwa-cockpit-context-data-buffer context)))
    (niwa-tools-list--collect-items session data-buffer)))

(defun niwa-tools-list-refresh ()
  "Refresh the current tools listing buffer."
  (interactive)
  (niwa-cockpit-surface-refresh))

(defun niwa-tools-list--detail-report (item &optional _context)
  "Return the information report for tool ITEM."
  (list :title "Tool" :subtitle (format "%s [%s]" (plist-get item :name) (plist-get item :state))
        :identity (plist-get item :name)
        :sections
        (list
         (list :id 'tool :title "Tool"
               :body (niwa-report-fields
                      (list "Name" (plist-get item :name))
                      (list "State" (plist-get item :state))
                      (list "Category" (or (plist-get item :category) ""))))
         (list :id 'description :title "Description" :mode 'markdown-mode
               :body (or (plist-get item :description) "")))))

(defun niwa-tools-list-details ()
  "Show details for the tool row at point."
  (interactive)
  (niwa-cockpit-surface-details))

(defun niwa-tools-list-search (&optional query)
  "Retrieve discoverable tool contracts matching QUERY."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (or (niwa-cockpit-context-session context)
                      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                                   "No niwa session in this buffer")))
         (data-buffer (niwa-tools-list--context-data-buffer))
         (candidates (delete-dups
                      (mapcar #'cadar
                              (niwa-session-tool-catalog session))))
         (query (or query
                    (completing-read
                     "Search tool contracts: "
                     candidates nil nil nil nil (car candidates))))
         result)
    (when (string-empty-p (string-trim query))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "Search query cannot be empty"))
    (with-current-buffer data-buffer
      (let ((niwa--agent-invocation nil)
            (niwa--session session))
        (niwa-tools--tool-search
         (lambda (text) (setq result text))
         query)))
    (niwa-tools-list-refresh)
    (message "%s" result)
    result))

(defun niwa-tools-list-open-gptel ()
  "Open gptel-menu from the tools listing's data buffer."
  (interactive)
  (require 'gptel-transient)
  (let ((data-buffer (niwa-tools-list--context-data-buffer)))
    (with-current-buffer data-buffer
      (call-interactively #'gptel-menu))))

(defun niwa-tools-list--help-report (&optional _context)
  "Return the complete cockpit help report."
  (list :title "Tools help"
        :sections (list (list :id 'keys :title "Keys"
                              :body (niwa-cockpit-surface-key-help-text niwa-tools-list--surface))
			(list :id 'rows :title "Rows" :body "active    Native tools in the current payload\ndiscoverable  Contracts through ToolSearch; calls through ToolCall"))))

(defun niwa-tools-list-help ()
  "Open tools cockpit help."
  (interactive)
  (niwa-cockpit-show-help
   niwa-tools-help-buffer-name
   (niwa-tools-list--help-report)))

(defun niwa-tools-list-quit ()
  "Close the tools cockpit and restore the invoking layout."
  (interactive)
  (niwa-cockpit-quit "tools cockpit"))

(defconst niwa-tools-list--surface
  `(:buffer-name ,niwa-tools-list-buffer-name
    :label "tools cockpit"
    :row-label "tool"
    :mode niwa-tools-list-mode
    :format [("State" 10 t)
             ("Name" 24 t)
             ("Category" 16 t)
             ("Description" 0 t)]
    :sort-key ("Name" . nil)
    :require-session t
    :collect niwa-tools-list--collect
    :entry niwa-tools-list--entry
    :header niwa-tools-list--header-line
    :details niwa-tools-list--detail-report
    :details-buffer "*niwa tool details*"
    :help-buffer ,niwa-tools-help-buffer-name
    :help-function niwa-tools-list--help-report
    :keys (("s" "Search tool contracts"
            niwa-tools-list-search)
           ("G" "Open gptel menu from the owning data buffer"
            niwa-tools-list-open-gptel)))
  "Cockpit surface spec for the tools list.")

(define-derived-mode niwa-tools-list-mode tabulated-list-mode
  "niwa-tools"
  "Major mode for managing niwa tool state."
  (niwa-cockpit-setup-tabulated-surface
   niwa-tools-list--surface))

(defun niwa-tools-list-open (&optional context)
  "Open the tools listing buffer for CONTEXT."
  (require 'niwa-tools)
  (let ((context (or context (niwa-cockpit-current-context))))
    (unless (niwa-cockpit-context-session context)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No niwa session in this buffer"))
    (niwa-cockpit-open-surface niwa-tools-list--surface context)))

(provide 'niwa-tools-list)
;;; niwa-tools-list.el ends here
