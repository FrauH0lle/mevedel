;;; mevedel-tools-list.el -- Tools cockpit list -*- lexical-binding: t -*-

;;; Commentary:

;; Native tools cockpit surface for inspecting and changing session-local tool
;; state.  The underlying deferred-tool mechanics live in `mevedel-tools';
;; this module owns only the tabulated UI.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

(require 'subr-x)
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
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-structs'
(declare-function mevedel-session-deferred-expired
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-deferred-injected
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-deferred-pending
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-deferred-set
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-deferred-used
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--session)

;; `mevedel-tools'
(declare-function mevedel-tools--tool-search
                  "mevedel-tools" (callback query &optional load))
(defvar mevedel-deferred-tool-ttl)

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
       ('deferred 'shadow)
       ('pending 'warning)
       ('loaded 'font-lock-keyword-face)
       ('expired 'error)
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
    (truncate-string-to-width (string-trim one-line) 96 nil nil "...")))

(defun mevedel-tools-list--tool-item (state tool)
  "Return a tools cockpit item for TOOL in STATE."
  (let ((description (gptel-tool-description tool)))
    (list :state state
          :name (gptel-tool-name tool)
          :category (or (gptel-tool-category tool) "")
          :ttl ""
          :description (if (stringp description) description "")
          :tool tool)))

(defun mevedel-tools-list--deferred-item (entry)
  "Return a tools cockpit item for deferred ENTRY."
  (pcase-let ((`((,category ,name) . ,summary) entry))
    (list :state 'deferred
          :name name
          :category (or category "")
          :ttl ""
          :description (if (stringp summary) summary "")
          :entry entry)))

(defun mevedel-tools-list--loaded-item (entry)
  "Return a tools cockpit item for loaded deferred ENTRY."
  (list :state 'loaded
        :name (car entry)
        :category ""
        :ttl (format "%s" (cdr entry))
        :description "Temporarily loaded deferred tool"
        :entry entry))

(defun mevedel-tools-list--expired-item (name)
  "Return a tools cockpit item for expired tool NAME."
  (list :state 'expired
        :name name
        :category ""
        :ttl ""
        :description "Expired after its deferred-tool TTL elapsed"))

(defun mevedel-tools-list--collect-items (session data-buffer)
  "Return tabulated tools cockpit items for SESSION and DATA-BUFFER."
  (let ((active (and (buffer-live-p data-buffer)
                     (with-current-buffer data-buffer
                       (and (boundp 'gptel-tools) gptel-tools))))
        (deferred (and session (mevedel-session-deferred-set session)))
        (pending (and session (mevedel-session-deferred-pending session)))
        (loaded (and session (mevedel-session-deferred-injected session)))
        (expired (and session (mevedel-session-deferred-expired session))))
    (append
     (mapcar (lambda (tool) (mevedel-tools-list--tool-item 'active tool))
             active)
     (mapcar #'mevedel-tools-list--deferred-item deferred)
     (mapcar (lambda (tool) (mevedel-tools-list--tool-item 'pending tool))
             pending)
     (mapcar #'mevedel-tools-list--loaded-item loaded)
     (mapcar #'mevedel-tools-list--expired-item expired))))

(defun mevedel-tools-list--entry (item &optional _context)
  "Return a `tabulated-list-mode' row for ITEM."
  (list
   (mevedel-tools-list--item-id item)
   (vector
    (mevedel-tools-list--status-cell (plist-get item :state))
    (plist-get item :name)
    (format "%s" (or (plist-get item :category) ""))
    (format "%s" (or (plist-get item :ttl) ""))
    (mevedel-tools-list--description-cell item))))

(defun mevedel-tools-list--session-label (&optional context)
  "Return CONTEXT's tools cockpit session label."
  (if-let* ((session (and context
                          (mevedel-cockpit-context-session context))))
      (mevedel-session-name session)
    "unknown"))

(defun mevedel-tools-list--header-line (&optional items context)
  "Return the tools cockpit header line for ITEMS and CONTEXT."
  (require 'mevedel-tools)
  (let ((counts nil))
    (dolist (item items)
      (cl-incf (alist-get (plist-get item :state) counts 0)))
    (format (concat "%s  %s  default TTL:%d  "
                    "active:%d deferred:%d pending:%d loaded:%d "
                    "expired:%d    RET details  d defer  a activate  "
                    "l load  G gptel  g refresh  ? help  q back")
            (propertize "mevedel: tools"
                        'face 'font-lock-function-name-face)
            (mevedel-tools-list--session-label context)
            mevedel-deferred-tool-ttl
            (alist-get 'active counts 0)
            (alist-get 'deferred counts 0)
            (alist-get 'pending counts 0)
            (alist-get 'loaded counts 0)
            (alist-get 'expired counts 0))))

(defun mevedel-tools-list--context ()
  "Return the current tools cockpit context."
  (require 'mevedel-cockpit)
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
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-refresh))

(defun mevedel-tools-list--selected-item ()
  "Return the selected tools cockpit item, or nil."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-selected t))

(defun mevedel-tools-list--selected-item-for-state (state)
  "Return the selected tools cockpit item when its state is STATE."
  (condition-case nil
      (when-let* ((item (mevedel-tools-list--selected-item)))
        (and (eq (plist-get item :state) state)
             item))
    (user-error nil)))

(defun mevedel-tools-list--detail-text (item &optional _context)
  "Return detail text for tools cockpit ITEM."
  (format (concat "Tool %s [%s]\nCategory: %s\nTTL: %s\n\n"
                  "Description:\n%s")
          (plist-get item :name)
          (symbol-name (plist-get item :state))
          (or (plist-get item :category) "")
          (or (plist-get item :ttl) "")
          (or (plist-get item :description) "")))

(defun mevedel-tools-list-details ()
  "Show details for the tool row at point."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-details))

(defun mevedel-tools-list--main-data-buffer ()
  "Return the data buffer for session-local lifecycle changes."
  (let ((data-buffer (mevedel-tools-list--context-data-buffer)))
    (with-current-buffer data-buffer
      (when (and (boundp 'mevedel--agent-invocation)
                 (mevedel-agent-invocation-p mevedel--agent-invocation))
        (user-error "Tool lifecycle actions are only supported for main sessions")))
    data-buffer))

(defun mevedel-tools-list--clear-runtime-state (session name)
  "Forget pending and loaded deferred state for tool NAME in SESSION."
  (setf (mevedel-session-deferred-pending session)
        (cl-remove name (mevedel-session-deferred-pending session)
                   :key #'gptel-tool-name :test #'equal))
  (setf (mevedel-session-deferred-injected session)
        (assoc-delete-all name (mevedel-session-deferred-injected session)
                          #'equal))
  (setf (mevedel-session-deferred-used session)
        (remove name (mevedel-session-deferred-used session)))
  (setf (mevedel-session-deferred-expired session)
        (remove name (mevedel-session-deferred-expired session))))

(defun mevedel-tools-list-defer-active (&optional name)
  "Move active tool NAME into this session's deferred set."
  (interactive)
  (let* ((context (mevedel-tools-list--context))
         (session (or (mevedel-cockpit-context-session context)
                      (user-error "No mevedel session in this buffer")))
         (data-buffer (mevedel-tools-list--main-data-buffer))
         (active (with-current-buffer data-buffer
                   (and (boundp 'gptel-tools) gptel-tools)))
         (selected (and (null name)
                        (mevedel-tools-list--selected-item-for-state 'active)))
         (name (or name
                   (plist-get selected :name)
                   (completing-read
                    "Defer active tool: "
                    (mapcar #'gptel-tool-name active) nil t)))
         (tool (if selected
                   (cl-find-if
                    (lambda (tool)
                      (mevedel-tools-list--tool-matches-item-p tool selected))
                    active)
                 (cl-find name active :key #'gptel-tool-name :test #'equal))))
    (unless tool
      (user-error "No active tool named %s" name))
    (with-current-buffer data-buffer
      (setq-local
       gptel-tools
       (if selected
           (cl-remove-if
            (lambda (tool)
              (mevedel-tools-list--tool-matches-item-p tool selected))
            gptel-tools)
         (cl-remove name gptel-tools
                    :key #'gptel-tool-name :test #'equal))))
    (let ((entry (cons (list (gptel-tool-category tool) name)
                       (gptel-tool-description tool))))
      (setf (mevedel-session-deferred-set session)
            (cons entry
                  (if selected
                      (cl-remove (car entry)
                                 (mevedel-session-deferred-set session)
                                 :key #'car :test #'equal)
                    (cl-remove name (mevedel-session-deferred-set session)
                               :key #'cadar :test #'equal)))))
    (mevedel-tools-list--clear-runtime-state session name)
    (mevedel-tools-list-refresh)
    (message "mevedel: deferred %s for this session" name)))

(defun mevedel-tools-list-activate-deferred (&optional name)
  "Move deferred tool NAME into this session's active tools."
  (interactive)
  (let* ((context (mevedel-tools-list--context))
         (session (or (mevedel-cockpit-context-session context)
                      (user-error "No mevedel session in this buffer")))
         (data-buffer (mevedel-tools-list--main-data-buffer))
         (deferred (mevedel-session-deferred-set session))
         (selected (and (null name)
                        (mevedel-tools-list--selected-item-for-state
                         'deferred)))
         (name (or name
                   (plist-get selected :name)
                   (completing-read
                    "Activate deferred tool: "
                    (mapcar #'cadar deferred) nil t)))
         (entry (or (plist-get selected :entry)
                    (cl-find name deferred :key #'cadar :test #'equal)))
         (tool (and entry (ignore-errors (gptel-get-tool (car entry))))))
    (unless tool
      (user-error "No deferred tool named %s" name))
    (setf (mevedel-session-deferred-set session)
          (if selected
              (cl-remove entry deferred :test #'equal)
            (cl-remove name deferred :key #'cadar :test #'equal)))
    (mevedel-tools-list--clear-runtime-state session name)
    (with-current-buffer data-buffer
      (unless (if selected
                  (cl-find-if
                   (lambda (tool)
                     (mevedel-tools-list--tool-matches-item-p tool selected))
                   gptel-tools)
                (cl-find name gptel-tools
                         :key #'gptel-tool-name :test #'equal))
        (setq-local gptel-tools (cons tool gptel-tools))))
    (mevedel-tools-list-refresh)
    (message "mevedel: activated %s for this session" name)))

(defun mevedel-tools-list-search-load (&optional query)
  "Search deferred tools by QUERY and queue matching tools for loading."
  (interactive)
  (require 'mevedel-tools)
  (let* ((context (mevedel-tools-list--context))
         (session (or (mevedel-cockpit-context-session context)
                      (user-error "No mevedel session in this buffer")))
         (data-buffer (mevedel-tools-list--main-data-buffer))
         (candidates (delete-dups
                      (mapcar #'cadar
                              (mevedel-session-deferred-set session))))
         (query (or query
                    (completing-read
                     "Search/load deferred tool: "
                     candidates nil nil nil nil (car candidates))))
         result)
    (when (string-empty-p (string-trim query))
      (user-error "Search query cannot be empty"))
    (with-current-buffer data-buffer
      (let ((mevedel--agent-invocation nil)
            (mevedel--session session))
        (mevedel-tools--tool-search
         (lambda (text) (setq result text))
         query t)))
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

(defconst mevedel-tools-list--help-text
  "mevedel tools cockpit

Keys
RET  Show selected tool details
d    Defer selected active tool, or prompt for an active tool
a    Activate selected deferred tool, or prompt for a deferred tool
l    Search and load deferred tools temporarily
G    Open gptel menu from the owning data buffer
g    Refresh tools table
?    Show this help
q    Back to the main session cockpit

Rows
active    Available in the current tool payload
deferred  Discoverable through ToolSearch
pending   Queued for temporary load on the next payload update
loaded    Temporarily loaded deferred tool with remaining TTL
expired   Expired on the previous payload update
"
  "Help text for the tools cockpit.")

(defun mevedel-tools-list-help ()
  "Open tools cockpit help."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-show-help
   mevedel-tools-help-buffer-name
   mevedel-tools-list--help-text))

(defun mevedel-tools-list-quit ()
  "Quit the tools cockpit and return to the main session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "tools cockpit"))

(defconst mevedel-tools-list--surface
  `(:buffer-name ,mevedel-tools-list-buffer-name
    :label "tools cockpit"
    :row-label "tool"
    :mode mevedel-tools-list-mode
    :format [("State" 10 t)
             ("Name" 24 t)
             ("Category" 16 t)
             ("TTL" 6 t)
             ("Description" 0 t)]
    :sort-key ("Name" . nil)
    :require-session t
    :collect mevedel-tools-list--collect
    :entry mevedel-tools-list--entry
    :header mevedel-tools-list--header-line
    :details mevedel-tools-list--detail-text
    :details-buffer "*mevedel tool details*"
    :help-buffer ,mevedel-tools-help-buffer-name
    :help-text ,mevedel-tools-list--help-text
    :keys (("a" . mevedel-tools-list-activate-deferred)
           ("d" . mevedel-tools-list-defer-active)
           ("l" . mevedel-tools-list-search-load)
           ("G" . mevedel-tools-list-open-gptel)))
  "Cockpit surface spec for the tools list.")

(define-derived-mode mevedel-tools-list-mode tabulated-list-mode
  "mevedel-tools"
  "Major mode for managing mevedel tool state."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-tools-list--surface))

(defun mevedel-tools-list-open (&optional context)
  "Open the tools listing buffer for CONTEXT."
  (require 'mevedel-cockpit)
  (require 'mevedel-tools)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (unless (mevedel-cockpit-context-session context)
      (user-error "No mevedel session in this buffer"))
    (mevedel-cockpit-open-surface mevedel-tools-list--surface context)))

(provide 'mevedel-tools-list)
;;; mevedel-tools-list.el ends here
