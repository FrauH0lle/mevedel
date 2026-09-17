;;; mevedel-permissions-list.el -- Remembered authority cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated cockpit surface for remembered permission authority.  Session,
;; workspace, and target-global rules and resource grants appear as selectable
;; rows.  Each row can be revoked without touching the other scopes.

;;; Code:

(require 'mevedel-report)

(require 'mevedel-cockpit)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-call-in-data
                  "mevedel-cockpit" (context function &rest args))
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

;; `mevedel-permission-persistence'
(declare-function mevedel-permission-persistence-file
                  "mevedel-permission-persistence" (workspace scope))
(declare-function mevedel-permission-persistent-authority
                  "mevedel-permission-persistence" (workspace scope))
(declare-function mevedel-permission-remove-persistent-resource-grant
                  "mevedel-permission-persistence"
                  (workspace scope path access &optional recursive))
(declare-function mevedel-permission-remove-persistent-rule
                  "mevedel-permission-persistence" (workspace scope rule))
(declare-function mevedel-permission-validate-persistent-stores
                  "mevedel-permission-persistence" (workspace))
(autoload 'mevedel-permission-persistence-file "mevedel-permission-persistence")
(autoload 'mevedel-permission-persistent-authority
  "mevedel-permission-persistence")
(autoload 'mevedel-permission-remove-persistent-resource-grant
  "mevedel-permission-persistence")
(autoload 'mevedel-permission-remove-persistent-rule
  "mevedel-permission-persistence")
(autoload 'mevedel-permission-validate-persistent-stores
  "mevedel-permission-persistence")

;; `mevedel-permissions'
(declare-function mevedel-permission-remove-session-resource-grant
                  "mevedel-permissions"
                  (session path access &optional recursive))
(declare-function mevedel-permission-remove-session-rule
                  "mevedel-permissions" (session rule))
(autoload 'mevedel-permission-remove-session-resource-grant
  "mevedel-permissions")
(autoload 'mevedel-permission-remove-session-rule "mevedel-permissions")

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-resource-grants "mevedel-structs" (cl-x) t)

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p "mevedel-transport" (&optional path))
(autoload 'mevedel-transport-busy-p "mevedel-transport")

(defconst mevedel-permissions-list-buffer-name "*mevedel permissions*"
  "Name of the remembered authority cockpit buffer.")

(defconst mevedel-permissions-list-help-buffer-name
  "*mevedel permissions help*"
  "Name of the remembered authority cockpit help buffer.")


;;
;;; Items

(defun mevedel-permissions-list--grant-spec (grant)
  "Return GRANT's path with its exact or recursive extent."
  (let ((path (plist-get grant :path)))
    (concat (if (stringp path) (abbreviate-file-name path) (format "%S" path))
            (if (plist-get grant :recursive) " (recursive)" " (exact)"))))

(defun mevedel-permissions-list--rule-item (scope rule)
  "Return the cockpit item for SCOPE permission RULE.
An execution profile's remembered child grants follow the pattern."
  (let ((plist (cdr rule)))
    (list :scope scope
          :kind (if (plist-get plist :network) 'network 'operation)
          :access (or (plist-get plist :action) 'allow)
          :subject (car rule)
          :spec (concat
                 (or (when-let* ((expression (plist-get plist :expression)))
                       (concat expression " (literal)"))
                     (plist-get plist :pattern)
                     (plist-get plist :path)
                     (plist-get plist :domain)
                     (plist-get plist :name)
                     "*")
                 (when-let* ((grants (plist-get plist :file-system)))
                   (format " [with command: %s]"
                           (mapconcat
                            (lambda (grant)
                              (format "%s %s" (plist-get grant :access)
                                      (mevedel-permissions-list--grant-spec grant)))
                            grants ", "))))
          :value rule)))

(defun mevedel-permissions-list--resource-item (scope grant)
  "Return the cockpit item for SCOPE resource GRANT."
  (list :scope scope
        :kind 'resource
        :access (plist-get grant :access)
        :subject "path"
        :spec (mevedel-permissions-list--grant-spec grant)
        :value grant))

(defun mevedel-permissions-list--collect (context)
  "Return remembered authority items for CONTEXT."
  (let* ((session (mevedel-cockpit-context-session context))
         (workspace (mevedel-cockpit-context-workspace context))
         items)
    (when workspace
      (when (mevedel-transport-busy-p
             (mevedel-permission-persistence-file workspace 'workspace))
        (user-error "Execution target is busy; refresh permissions when idle"))
      (mevedel-cockpit-call-in-data
       context #'mevedel-permission-validate-persistent-stores workspace))
    (dolist (rule (and session (mevedel-session-permission-rules session)))
      (push (mevedel-permissions-list--rule-item 'session rule) items))
    (dolist (grant (and session (mevedel-session-resource-grants session)))
      (push (mevedel-permissions-list--resource-item 'session grant) items))
    (when workspace
      (dolist (scope '(workspace global))
        (let ((persistent
               (mevedel-cockpit-call-in-data
                context #'mevedel-permission-persistent-authority workspace scope)))
          (dolist (rule (plist-get persistent :rules))
            (push (mevedel-permissions-list--rule-item scope rule) items))
          (dolist (grant (plist-get persistent :resource-grants))
            (push (mevedel-permissions-list--resource-item scope grant) items)))))
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
                     'face (if (memq (plist-get item :scope) '(workspace global))
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
         (format "%d remembered · %d session · %d workspace · %d global"
                 (length items)
                 (funcall scoped 'session)
                 (funcall scoped 'workspace)
                 (funcall scoped 'global))
       "nothing remembered"))))

(defun mevedel-permissions-list--details (item context)
  "Return the information report for authority ITEM."
  (list :title "Remembered authority" :subtitle (format "%s" (plist-get item :subject))
        :identity (plist-get item :value)
        :sections
        (list
         (list :id 'authority :title "Authority"
               :body (mevedel-report-fields
                      (list "Scope" (plist-get item :scope))
                      (list "Store"
                            (if (eq (plist-get item :scope) 'session)
                                "Session sidecar"
                              (mevedel-cockpit-call-in-data
                               context #'mevedel-permission-persistence-file
                               (mevedel-cockpit-context-workspace context)
                               (plist-get item :scope))))
                      (list "Kind" (plist-get item :kind))
                      (list "Access" (plist-get item :access))
                      (list "Subject" (plist-get item :subject))
                      (list "Spec" (plist-get item :spec))))
         (list :id 'record :title "Record" :mode 'emacs-lisp-mode :folded t
               :body (format "%S" (plist-get item :value))))))


;;
;;; Actions

(defun mevedel-permissions-list-revoke ()
  "Revoke the selected remembered authority."
  (interactive)
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
        session (plist-get value :path) (plist-get value :access)
        (plist-get value :recursive)))
      (`(,(and scope (or 'workspace 'global)) . rule)
       (mevedel-cockpit-call-in-data
        context #'mevedel-permission-remove-persistent-rule workspace scope value))
      (`(,(and scope (or 'workspace 'global)) . resource)
       (mevedel-cockpit-call-in-data
        context #'mevedel-permission-remove-persistent-resource-grant
        workspace scope (plist-get value :path) (plist-get value :access)
        (plist-get value :recursive))))
    (mevedel-cockpit-surface-refresh)
    (message "mevedel: revoked %s" label)))

(defun mevedel-permissions-list-quit ()
  "Quit the permissions cockpit and return to the session cockpit."
  (interactive)
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
    :help-function mevedel-permissions-list--help-report
    :keys (("d" "Revoke the selected authority"
            mevedel-permissions-list-revoke)))
  "Cockpit surface spec for remembered permission authority.")

(defun mevedel-permissions-list--help-report (&optional _context)
  "Return the complete cockpit help report."
  (list :title "Permissions help"
        :sections (list (list :id 'keys :title "Keys"
                              :body (mevedel-cockpit-surface-key-help-text mevedel-permissions-list--surface))
			(list :id 'rows :title "Rows" :body "operation  Tool authority remembered for a matching operation\nnetwork    Operation authority that also carries network access\nresource   Exact or recursive path grant remembered for one access mode\nsession    Held by this session only, and saved with it\nworkspace  Shared by every session in this workspace\nglobal     Authored in the execution target user's permissions file\n\nPrompt approvals remember session or workspace authority.\nGlobal entries are authored explicitly. Revoking a global entry affects\nall projects on that target. Other entries may still grant the same access."))))

(define-derived-mode mevedel-permissions-list-mode tabulated-list-mode
  "mevedel-permissions"
  "Major mode for inspecting and revoking remembered permission authority."
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-permissions-list--surface))

(defun mevedel-permissions-list-open (&optional context)
  "Open the remembered authority cockpit for CONTEXT."
  (interactive)
  (mevedel-cockpit-open-surface
   mevedel-permissions-list--surface
   (or context (mevedel-cockpit-current-context))))

(provide 'mevedel-permissions-list)

;;; mevedel-permissions-list.el ends here
