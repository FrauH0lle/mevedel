;;; mevedel-permissions-list.el -- Remembered authority cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated cockpit surface for remembered permission authority.  Session
;; and workspace operation rules, network-qualified rules, and exact resource
;; grants appear as selectable rows, and each row can be revoked on its own
;; without touching the others.

;;; Code:

(eval-when-compile
  (require 'tabulated-list))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-workspace
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-format-header
                  "mevedel-cockpit" (name scope state))
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-key-help-text
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-permissions'
(declare-function mevedel-permission-persistent-authority
                  "mevedel-permissions" (workspace))
(declare-function mevedel-permission-remove-persistent-resource-grant
                  "mevedel-permissions" (workspace path access))
(declare-function mevedel-permission-remove-persistent-rule
                  "mevedel-permissions" (workspace rule))
(declare-function mevedel-permission-remove-session-resource-grant
                  "mevedel-permissions" (session path access))
(declare-function mevedel-permission-remove-session-rule
                  "mevedel-permissions" (session rule))

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x) t)

(defconst mevedel-permissions-list-buffer-name "*mevedel permissions*"
  "Name of the remembered authority cockpit buffer.")

(defconst mevedel-permissions-list-help-buffer-name
  "*mevedel permissions help*"
  "Name of the remembered authority cockpit help buffer.")


;;
;;; Items

(defun mevedel-permissions-list--rule-item (scope rule)
  "Return the cockpit item for SCOPE permission RULE."
  (let ((plist (cdr rule)))
    (list :scope scope
          :kind (if (plist-get plist :network) 'network 'operation)
          :access (or (plist-get plist :action) 'allow)
          :subject (car rule)
          :spec (or (plist-get plist :pattern)
                    (plist-get plist :path)
                    (plist-get plist :domain)
                    (plist-get plist :name)
                    "*")
          :value rule)))

(defun mevedel-permissions-list--resource-item (scope grant)
  "Return the cockpit item for SCOPE exact resource GRANT."
  (let ((path (plist-get grant :path)))
    (list :scope scope
          :kind 'resource
          :access (plist-get grant :access)
          :subject "path"
          :spec (if (stringp path)
                    (abbreviate-file-name path)
                  (format "%S" path))
          :value grant)))

(defun mevedel-permissions-list--collect (context)
  "Return remembered authority items for CONTEXT."
  (require 'mevedel-permissions)
  (let* ((session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         (persistent (and workspace
                          (mevedel-permission-persistent-authority workspace)))
         items)
    (dolist (rule (and session (mevedel-session-permission-rules session)))
      (push (mevedel-permissions-list--rule-item 'session rule) items))
    (dolist (grant (and session (mevedel-session-resource-grants session)))
      (push (mevedel-permissions-list--resource-item 'session grant) items))
    (dolist (rule (plist-get persistent :rules))
      (push (mevedel-permissions-list--rule-item 'workspace rule) items))
    (dolist (grant (plist-get persistent :resource-grants))
      (push (mevedel-permissions-list--resource-item 'workspace grant) items))
    (nreverse items)))

(defun mevedel-permissions-list--label (item)
  "Return ITEM's one-line authority label."
  (format "%s %s: %s %s"
          (plist-get item :scope)
          (plist-get item :kind)
          (plist-get item :subject)
          (plist-get item :spec)))

(defun mevedel-permissions-list--entry (item _context)
  "Return the tabulated row for authority ITEM."
  (list (mevedel-permissions-list--label item)
        (vector
         (propertize (format "%s" (plist-get item :scope))
                     'face (if (eq (plist-get item :scope) 'workspace)
                               'warning
                             'default))
         (format "%s" (plist-get item :kind))
         (format "%s" (plist-get item :access))
         (format "%-11s %s"
                 (plist-get item :subject)
                 (plist-get item :spec)))))

(defun mevedel-permissions-list--header (items context)
  "Return the cockpit header line for authority ITEMS and CONTEXT."
  (let ((session (mevedel-cockpit-context-session context))
        (scoped (lambda (scope)
                  (seq-count (lambda (item)
                               (eq (plist-get item :scope) scope))
                             items))))
    (mevedel-cockpit-format-header
     "permissions"
     (if session (mevedel-session-name session) "")
     (if items
         (format "%d remembered · %d session · %d workspace"
                 (length items)
                 (funcall scoped 'session)
                 (funcall scoped 'workspace))
       "nothing remembered"))))

(defun mevedel-permissions-list--details (item _context)
  "Return the detail text for authority ITEM."
  (string-join
   (list "mevedel remembered authority"
         ""
         (format "Scope     %s" (plist-get item :scope))
         (format "Kind      %s" (plist-get item :kind))
         (format "Access    %s" (plist-get item :access))
         (format "Subject   %s" (plist-get item :subject))
         (format "Spec      %s" (plist-get item :spec))
         ""
         (format "Record    %S" (plist-get item :value))
         "")
   "\n"))


;;
;;; Actions

(defun mevedel-permissions-list-revoke ()
  "Revoke the selected remembered authority."
  (interactive)
  (require 'mevedel-permissions)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         (item (mevedel-cockpit-surface-selected))
         (value (plist-get item :value))
         (label (mevedel-permissions-list--label item)))
    (pcase (cons (plist-get item :scope)
                 (if (eq (plist-get item :kind) 'resource) 'resource 'rule))
      (`(session . rule)
       (mevedel-permission-remove-session-rule session value))
      (`(session . resource)
       (mevedel-permission-remove-session-resource-grant
        session (plist-get value :path) (plist-get value :access)))
      (`(workspace . rule)
       (mevedel-permission-remove-persistent-rule workspace value))
      (`(workspace . resource)
       (mevedel-permission-remove-persistent-resource-grant
        workspace (plist-get value :path) (plist-get value :access))))
    (mevedel-cockpit-surface-refresh)
    (message "mevedel: revoked %s" label)))

(defun mevedel-permissions-list-quit ()
  "Quit the permissions cockpit and return to the session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "permissions cockpit"))


;;
;;; Surface

(defconst mevedel-permissions-list--surface
  `(:buffer-name ,mevedel-permissions-list-buffer-name
    :label "permissions cockpit"
    :row-label "authority"
    :mode mevedel-permissions-list-mode
    :format [("Scope" 10 t)
             ("Kind" 10 t)
             ("Access" 8 t)
             ("Subject" 0 t)]
    :sort-key ("Scope" . nil)
    :require-session t
    :collect mevedel-permissions-list--collect
    :entry mevedel-permissions-list--entry
    :header mevedel-permissions-list--header
    :details mevedel-permissions-list--details
    :details-buffer "*mevedel authority details*"
    :help-buffer ,mevedel-permissions-list-help-buffer-name
    :help-function mevedel-permissions-list--help-text
    :keys (("d" "Revoke the selected authority"
            mevedel-permissions-list-revoke)))
  "Cockpit surface spec for remembered permission authority.")

(defun mevedel-permissions-list--help-text (&optional _context)
  "Return help text for the remembered authority cockpit."
  (string-join
   (list "mevedel permissions cockpit"
         ""
         "Keys"
         (mevedel-cockpit-surface-key-help-text
          mevedel-permissions-list--surface)
         ""
         "Rows"
         "operation  Tool authority remembered for a matching operation"
         "network    Operation authority that also carries network access"
         "resource   Exact path grant remembered for one access mode"
         "session    Held by this session only, and saved with it"
         "workspace  Shared by every session in this workspace"
         ""
         "Entries appear here when an authority prompt is answered with"
         "\"Always\".  Revoking one leaves the others untouched."
         "")
   "\n"))

(define-derived-mode mevedel-permissions-list-mode tabulated-list-mode
  "mevedel-permissions"
  "Major mode for inspecting and revoking remembered permission authority."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-permissions-list--surface))

(defun mevedel-permissions-list-open (&optional context)
  "Open the remembered authority cockpit for CONTEXT."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-open-surface
   mevedel-permissions-list--surface
   (or context (mevedel-cockpit-current-context))))

(provide 'mevedel-permissions-list)

;;; mevedel-permissions-list.el ends here
