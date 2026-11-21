;;; niwa-permissions-list.el -- Remembered authority cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Tabulated cockpit surface for remembered permission authority.  Session,
;; workspace, and target-global rules and resource grants appear as selectable
;; rows, as do the safe environment variables whose leading assignments Bash
;; rule matching ignores.  Each stored row can be revoked without touching the
;; other scopes; the built-in safe variables share one read-only row.

;;; Code:

(require 'niwa-report)

(require 'niwa-cockpit)
(require 'niwa-permission-rules)

;; `niwa-cockpit'
(declare-function niwa-cockpit-call-in-data
                  "niwa-cockpit" (context function &rest args))
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-key-help-text
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))

;; `niwa-permission-persistence'
(declare-function niwa-permission-persistence-file
                  "niwa-permission-persistence" (workspace scope))
(declare-function niwa-permission-persistent-authority
                  "niwa-permission-persistence" (workspace scope))
(declare-function niwa-permission-remove-persistent-resource-grant
                  "niwa-permission-persistence"
                  (workspace scope path access &optional recursive origin))
(declare-function niwa-permission-remove-persistent-rule
                  "niwa-permission-persistence" (workspace scope rule))
(declare-function niwa-permission-remove-persistent-safe-env-var
                  "niwa-permission-persistence" (workspace scope name))
(declare-function niwa-permission-validate-persistent-stores
                  "niwa-permission-persistence" (workspace))
(autoload 'niwa-permission-persistence-file "niwa-permission-persistence")
(autoload 'niwa-permission-persistent-authority
  "niwa-permission-persistence")
(autoload 'niwa-permission-remove-persistent-resource-grant
  "niwa-permission-persistence")
(autoload 'niwa-permission-remove-persistent-rule
  "niwa-permission-persistence")
(autoload 'niwa-permission-remove-persistent-safe-env-var
  "niwa-permission-persistence")
(autoload 'niwa-permission-validate-persistent-stores
  "niwa-permission-persistence")

;; `niwa-permissions'
(declare-function niwa-permission-remove-session-resource-grant
                  "niwa-permissions"
                  (session path access &optional recursive))
(declare-function niwa-permission-remove-session-rule
                  "niwa-permissions" (session rule))
(autoload 'niwa-permission-remove-session-resource-grant
  "niwa-permissions")
(autoload 'niwa-permission-remove-session-rule "niwa-permissions")

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-rules "niwa-structs" (cl-x) t)
(declare-function niwa-session-resource-grants "niwa-structs" (cl-x) t)

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))
(autoload 'niwa-transport-busy-p "niwa-transport")

(defconst niwa-permissions-list-buffer-name "*niwa permissions*"
  "Name of the remembered authority cockpit buffer.")

(defconst niwa-permissions-list-help-buffer-name
  "*niwa permissions help*"
  "Name of the remembered authority cockpit help buffer.")


;;
;;; Items

(defun niwa-permissions-list--grant-spec (grant)
  "Return GRANT's path with its exact or recursive extent."
  (let ((path (plist-get grant :path)))
    (concat (if-let* ((origin (plist-get grant :workspace-origin)))
                (format "%s [root %s]" (plist-get origin :relative)
                        (abbreviate-file-name (plist-get origin :root)))
              (if (stringp path) (abbreviate-file-name path) (format "%S" path)))
            (if (plist-get grant :recursive) " (recursive)" " (exact)"))))

(defun niwa-permissions-list--rule-item (scope rule)
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
                                      (niwa-permissions-list--grant-spec grant)))
                            grants ", "))))
          :value rule)))

(defun niwa-permissions-list--resource-item (scope grant)
  "Return the cockpit item for SCOPE resource GRANT."
  (list :scope scope
        :kind 'resource
        :access (plist-get grant :access)
        :subject "path"
        :spec (niwa-permissions-list--grant-spec grant)
        :value grant))

(defun niwa-permissions-list--safe-env-item (scope name)
  "Return the cockpit item for SCOPE's safe environment variable NAME.
SCOPE `built-in' lists every built-in variable in NAME as one row."
  (list :scope scope
        :kind 'safe-env
        :access 'ignored
        :subject "variable"
        :spec name
        :value name))

(defun niwa-permissions-list--collect (context)
  "Return remembered authority items for CONTEXT."
  (let* ((session (niwa-cockpit-context-session context))
         (workspace (niwa-cockpit-context-workspace context))
         items)
    (when workspace
      (when (niwa-transport-busy-p
             (niwa-permission-persistence-file workspace 'workspace))
        (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace)
                                     "Execution target is busy; refresh permissions when idle"))
      (niwa-cockpit-call-in-data
       context #'niwa-permission-validate-persistent-stores workspace))
    (dolist (rule (and session (niwa-session-permission-rules session)))
      (push (niwa-permissions-list--rule-item 'session rule) items))
    (dolist (grant (and session (niwa-session-resource-grants session)))
      (push (niwa-permissions-list--resource-item 'session grant) items))
    (when workspace
      (dolist (scope '(workspace global))
        (let ((persistent
               (niwa-cockpit-call-in-data
                context #'niwa-permission-persistent-authority workspace scope)))
          (dolist (rule (plist-get persistent :rules))
            (push (niwa-permissions-list--rule-item scope rule) items))
          (dolist (grant (plist-get persistent :resource-grants))
            (push (niwa-permissions-list--resource-item scope grant) items))
          (dolist (name (plist-get persistent :safe-env-vars))
            (push (niwa-permissions-list--safe-env-item scope name) items)))))
    (push (niwa-permissions-list--safe-env-item
           'built-in
           (string-join niwa-permission-rules-builtin-safe-env-vars " "))
          items)
    (nreverse items)))

(defun niwa-permissions-list--label (item)
  "Return ITEM's one-line authority label."
  (format "%s %s: %s %s"
          (plist-get item :scope)
          (plist-get item :kind)
          (plist-get item :subject)
          (plist-get item :spec)))

(defun niwa-permissions-list--entry (item _context)
  "Return the tabulated row for authority ITEM."
  (list (niwa-permissions-list--label item)
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

(defun niwa-permissions-list--header (items context)
  "Return the cockpit header line for authority ITEMS and CONTEXT."
  (let ((session (niwa-cockpit-context-session context))
        (items (seq-remove (lambda (item) (eq (plist-get item :scope) 'built-in))
                           items))
        (scoped (lambda (scope)
                  (seq-count (lambda (item)
                               (eq (plist-get item :scope) scope))
                             items))))
    (niwa-cockpit-format-header
     "permissions"
     (if session (niwa-session-name session) "")
     (if items
         (format "%d remembered · %d session · %d workspace · %d global"
                 (length items)
                 (funcall scoped 'session)
                 (funcall scoped 'workspace)
                 (funcall scoped 'global))
       "nothing remembered"))))

(defun niwa-permissions-list--details (item context)
  "Return the information report for authority ITEM."
  (list :title "Remembered authority" :subtitle (format "%s" (plist-get item :subject))
        :identity (plist-get item :value)
        :sections
        (list
         (list :id 'authority :title "Authority"
               :body (niwa-report-fields
                      (list "Scope" (plist-get item :scope))
                      (list "Store"
                            (pcase (plist-get item :scope)
                              ('session "Session sidecar")
                              ('built-in "Built into niwa")
                              (scope
                               (niwa-cockpit-call-in-data
                                context #'niwa-permission-persistence-file
                                (niwa-cockpit-context-workspace context)
                                scope))))
                      (list "Kind" (plist-get item :kind))
                      (list "Access" (plist-get item :access))
                      (list "Subject" (plist-get item :subject))
                      (list "Spec" (plist-get item :spec))))
         (list :id 'record :title "Record" :mode 'emacs-lisp-mode :folded t
               :body (format "%S" (plist-get item :value))))))


;;
;;; Actions

(defun niwa-permissions-list-revoke ()
  "Revoke the selected remembered authority."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (session (niwa-cockpit-context-session context))
         (workspace (niwa-cockpit-context-workspace context))
         (item (niwa-cockpit-surface-selected))
         (value (plist-get item :value))
         (label (niwa-permissions-list--label item)))
    (pcase (cons (plist-get item :scope)
                 (pcase (plist-get item :kind)
                   ('resource 'resource)
                   ('safe-env 'safe-env)
                   (_ 'rule)))
      (`(built-in . ,_)
       (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                     "Built-in safe variables cannot be removed"))
      (`(session . rule)
       (niwa-permission-remove-session-rule session value))
      (`(session . resource)
       (niwa-permission-remove-session-resource-grant
        session (plist-get value :path) (plist-get value :access)
        (plist-get value :recursive)))
      (`(,(and scope (or 'workspace 'global)) . rule)
       (niwa-cockpit-call-in-data
        context #'niwa-permission-remove-persistent-rule workspace scope value))
      (`(,(and scope (or 'workspace 'global)) . resource)
       (niwa-cockpit-call-in-data
        context #'niwa-permission-remove-persistent-resource-grant
        workspace scope (plist-get value :path) (plist-get value :access)
        (plist-get value :recursive) (plist-get value :workspace-origin)))
      (`(,(and scope (or 'workspace 'global)) . safe-env)
       (niwa-cockpit-call-in-data
        context #'niwa-permission-remove-persistent-safe-env-var
        workspace scope value)))
    (niwa-cockpit-surface-refresh)
    (message "niwa: revoked %s" label)))

(defun niwa-permissions-list-quit ()
  "Close the permissions cockpit and restore the invoking layout."
  (interactive)
  (niwa-cockpit-quit "permissions cockpit"))


;;
;;; Surface

(defconst niwa-permissions-list--surface
  `(:buffer-name ,niwa-permissions-list-buffer-name
    :label "permissions cockpit"
    :row-label "authority"
    :mode niwa-permissions-list-mode
    :format [("Scope" 10 t)
             ("Kind" 10 t)
             ("Access" 8 t)
             ("Subject" 0 t)]
    :sort-key ("Scope" . nil)
    :require-session t
    :collect niwa-permissions-list--collect
    :entry niwa-permissions-list--entry
    :header niwa-permissions-list--header
    :details niwa-permissions-list--details
    :details-buffer "*niwa authority details*"
    :help-buffer ,niwa-permissions-list-help-buffer-name
    :help-function niwa-permissions-list--help-report
    :keys (("d" "Revoke the selected authority"
            niwa-permissions-list-revoke)))
  "Cockpit surface spec for remembered permission authority.")

(defun niwa-permissions-list--help-report (&optional _context)
  "Return the complete cockpit help report."
  (list :title "Permissions help"
        :sections (list (list :id 'keys :title "Keys"
                              :body (niwa-cockpit-surface-key-help-text niwa-permissions-list--surface))
			(list :id 'rows :title "Rows" :body "operation  Tool authority remembered for a matching operation\nnetwork    Operation authority that also carries network access\nresource   Exact or recursive path grant remembered for one access mode\nsafe-env   Variable whose leading assignment Bash rule matching ignores\nsession    Held by this session only, and saved with it\nworkspace  Shared by every session in this workspace\nglobal     Authored in the execution target user's permissions file\nbuilt-in   Shipped with niwa; shown for reference and not removable\n\nPrompt approvals remember session or workspace authority.\nGlobal entries and safe variables are authored explicitly. Revoking a\nglobal entry affects all projects on that target. Other entries may\nstill grant the same access."))))

(define-derived-mode niwa-permissions-list-mode tabulated-list-mode
  "niwa-permissions"
  "Major mode for inspecting and revoking remembered permission authority."
  (niwa-cockpit-setup-tabulated-surface
   niwa-permissions-list--surface))

(defun niwa-permissions-list-open (&optional context)
  "Open the remembered authority cockpit for CONTEXT."
  (interactive)
  (niwa-cockpit-open-surface
   niwa-permissions-list--surface
   (or context (niwa-cockpit-current-context))))

(provide 'niwa-permissions-list)

;;; niwa-permissions-list.el ends here
