;;; test-niwa-permissions-list.el -- Tests for the authority cockpit -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "niwa-report-test-support"))

(require 'ert)
(require 'cl-lib)
(require 'tabulated-list)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'niwa-cockpit)
(require 'niwa-permission-persistence)
(require 'niwa-permissions)
(require 'niwa-permissions-list)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-workspace)

(defmacro niwa-permissions-list-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer and a live context.
Binds `context' to the cockpit context that owns the surface."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "niwa-authority-root-" t))
          (niwa-user-dir (file-name-as-directory
                             (make-temp-file "niwa-authority-user-" t)))
          (workspace (niwa-workspace-get-or-create
                      'project (format "authority-%s" root) root "niwa"))
          (session (niwa-session-create "main" workspace))
          (data-buf (generate-new-buffer " *authority-data*"))
          (view-buf (generate-new-buffer " *authority-view*"))
          context)
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (setq-local default-directory (file-name-as-directory root))
             (setq-local niwa--session session))
           (niwa-view--setup view-buf data-buf)
           (with-current-buffer view-buf
             (setq context (niwa-cockpit-current-context)))
           ,@body)
       (when-let* ((buffer (get-buffer niwa-permissions-list-buffer-name)))
         (kill-buffer buffer))
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p niwa-user-dir)
         (delete-directory niwa-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(defun niwa-permissions-list-test--open (context)
  "Open the authority cockpit for CONTEXT and return its buffer."
  (save-window-excursion
    (niwa-permissions-list-open context)))

(defun niwa-permissions-list-test--row (kind)
  "Move point to the first row whose Kind column is KIND.
Return the row's tabulated id."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when-let* ((entry (tabulated-list-get-entry))
                  ((equal (aref entry 1) kind)))
        (throw 'found (tabulated-list-get-id)))
      (forward-line 1))
    (error "No %s row in the authority cockpit" kind)))

(niwa-deftest niwa-permissions-list--collect ()
  ,test
  (test)
  :doc "shows a remembered Eval expression with its literal matching extent"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Eval" :expression "(message \"*\")" :action allow)))
    (should (equal "(message \"*\") (literal)"
                   (plist-get (car (niwa-permissions-list--collect context)) :spec))))
  :doc "distinguishes session operation, network, and resource authority"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)
            ("Bash" :pattern "npx test*" :network t :action allow)))
    (setf (niwa-session-resource-grants session)
          '((:path "/tmp/external" :access read)))
    (let ((items (niwa-permissions-list--collect context)))
      (should (equal '(operation network resource safe-env)
                     (mapcar (lambda (item) (plist-get item :kind)) items)))
      (should (equal '(session session session built-in)
                     (mapcar (lambda (item) (plist-get item :scope)) items)))))

  :doc "shows an execution profile's remembered child grants after its pattern"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Bash" :pattern "npm test" :action allow
             :file-system ((:path "/tmp/cache" :access write :recursive t)))))
    (should (equal "npm test [with command: write /tmp/cache (recursive)]"
                   (plist-get (car (niwa-permissions-list--collect context))
                              :spec))))

  :doc "collects workspace authority alongside session authority"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (niwa-permission-persistence-save-resource-grant
     workspace "/tmp/workspace-external" 'write)
    (let ((items (niwa-permissions-list--collect context)))
      (should (equal '(session workspace built-in)
                     (mapcar (lambda (item) (plist-get item :scope)) items)))
      (should (equal 'write (plist-get (nth 1 items) :access)))))

  :doc "lists store safe variables singly and built-in ones in one row"
  (niwa-permissions-list-test--with-buffers
    (niwa-permission-persistence-write-store
     (niwa-permission-persistence-file workspace 'workspace)
     '(:rules nil :resource-grants nil :safe-env-vars ("CHECK_BASE")))
    (niwa-permission-persistence-write-store
     (niwa-permission-persistence-file workspace 'global)
     '(:rules nil :resource-grants nil :safe-env-vars ("TOOL_QUIET")))
    (let ((rows (mapcar (lambda (item)
                          (list (plist-get item :scope) (plist-get item :kind)
                                (plist-get item :spec)))
                        (niwa-permissions-list--collect context))))
      (should (member '(workspace safe-env "CHECK_BASE") rows))
      (should (member '(global safe-env "TOOL_QUIET") rows))
      (should (seq-find (lambda (row)
                          (and (eq (car row) 'built-in)
                               (string-match-p "\\bLANG\\b" (nth 2 row))))
                        rows)))))

(niwa-deftest niwa-permissions-list--header ()
  ,test
  (test)
  :doc "counts remembered authority per scope"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (niwa-permission-persistence-save-resource-grant
     workspace "/tmp/workspace-external" 'write)
    (let ((header (substring-no-properties
                   (niwa-permissions-list--header
                    (niwa-permissions-list--collect context)
                    context))))
      (should (string-match-p "niwa: permissions" header))
      (should (string-match-p "2 remembered · 1 session · 1 workspace"
                              header))
      (should (string-match-p "? keys" header))))

  :doc "names the empty state instead of counting nothing"
  (niwa-permissions-list-test--with-buffers
    (should (string-match-p
             "nothing remembered"
             (substring-no-properties
              (niwa-permissions-list--header
               (niwa-permissions-list--collect context) context))))))

(niwa-deftest niwa-permissions-list-revoke ()
  ,test
  (test)
  :doc "shows global entries and revokes only their backing store"
  (niwa-permissions-list-test--with-buffers
    (let* ((global-file (niwa-permission-persistence-file workspace 'global))
           (rule '("Bash" :pattern "echo *" :action allow))
           (grant '(:path "/tmp/global-tree" :access read :recursive t))
           (store (list :rules (list rule) :resource-grants (list grant))))
      (niwa-permission-persistence-write-store global-file store)
      (niwa-permission-persistence-write-store
       (niwa-permission-persistence-file workspace 'workspace) store)
      (with-current-buffer (niwa-permissions-list-test--open context)
        (should (= 5 (length tabulated-list-entries)))
        (dolist (kind '("operation" "resource"))
          (goto-char (point-min))
          (while (not (or (eobp)
                          (when-let* ((entry (tabulated-list-get-entry)))
                            (and (equal (aref entry 0) "global")
                                 (equal (aref entry 1) kind)))))
            (forward-line 1))
          (should-not (eobp))
          (let ((item (niwa-cockpit-surface-selected)))
            (should (string-match-p
                     (regexp-quote global-file)
                     (niwa-report-test-text
                      (niwa-permissions-list--details item context)))))
          (niwa-test--with-captured-messages nil
            (niwa-permissions-list-revoke)))
        (should (= 3 (length tabulated-list-entries))))
      (should (equal '(:rules nil :resource-grants nil)
                     (niwa-permission-persistent-authority workspace 'global)))
      (should (equal store (niwa-permission-persistent-authority
                            workspace 'workspace)))))
  :doc "refuses revocation through a symlinked global file"
  (niwa-permissions-list-test--with-buffers
    (let* ((file (niwa-permission-persistence-file workspace 'global))
           (outside (file-name-concat niwa-user-dir "outside.el"))
           (store '(:rules (("Read" :action allow)) :resource-grants nil)))
      (niwa-permission-persistence-write-store outside store)
      (make-symbolic-link outside file)
      (with-current-buffer (niwa-permissions-list-test--open context)
        (niwa-permissions-list-test--row "operation")
        (should-error (niwa-permissions-list-revoke)))
      (should (file-symlink-p file))
      (should (equal store (plist-get
                           (niwa-permission--read-store-file-uncached outside)
                           :store)))))
  :doc "failed global replacement leaves authority intact and reports no success"
  (niwa-permissions-list-test--with-buffers
    (let* ((file (niwa-permission-persistence-file workspace 'global))
           (store '(:rules (("Read" :action allow)) :resource-grants nil))
           captured)
      (niwa-permission-persistence-write-store file store)
      (with-current-buffer (niwa-permissions-list-test--open context)
        (niwa-permissions-list-test--row "operation")
        (cl-letf (((symbol-function 'niwa-session-control-fs-write-file)
                   (lambda (&rest _) (error "Injected replacement failure"))))
          (niwa-test--with-captured-messages captured
            (should-error (niwa-permissions-list-revoke)))))
      (should-not (string-match-p "revoked" (format "%s" captured)))
      (should (equal store (niwa-permission-persistent-authority workspace 'global)))))
  :doc "revokes a session network rule without touching its siblings"
  (niwa-permissions-list-test--with-buffers
    (let ((operation '("Bash" :pattern "npx test*" :action allow))
          (network '("Bash" :pattern "npx test*" :network t :action allow))
          (resource '(:path "/tmp/external" :access read)))
      (setf (niwa-session-permission-rules session)
            (list operation network))
      (setf (niwa-session-resource-grants session) (list resource))
      (with-current-buffer (niwa-permissions-list-test--open context)
        (niwa-permissions-list-test--row "network")
        (niwa-test--with-captured-messages nil
          (niwa-permissions-list-revoke)))
      (should (equal (list operation)
                     (niwa-session-permission-rules session)))
      (should (equal (list resource)
                     (niwa-session-resource-grants session)))))

  :doc "revokes one workspace resource without touching workspace rules"
  (niwa-permissions-list-test--with-buffers
    (let ((network '("Bash" :pattern "npx test*" :network t :action allow)))
      (niwa-permission-persistence-save-rule
       workspace network)
      (niwa-permission-persistence-save-resource-grant
       workspace "/tmp/workspace-external" 'read)
      (with-current-buffer (niwa-permissions-list-test--open context)
        (niwa-permissions-list-test--row "resource")
        (niwa-test--with-captured-messages nil
          (niwa-permissions-list-revoke)))
      (let ((authority (niwa-permission-persistent-authority workspace 'workspace)))
        (should (equal (list network) (plist-get authority :rules)))
        (should-not (plist-get authority :resource-grants)))))

  :doc "leaves an active composer draft untouched"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (niwa-view-test--insert-composer-draft draft 4))
      (with-current-buffer (niwa-permissions-list-test--open context)
        (niwa-permissions-list-test--row "operation")
        (niwa-test--with-captured-messages nil
          (niwa-permissions-list-revoke)))
      (with-current-buffer view-buf
        (should (string= draft (niwa-view-composer-draft-text)))
        (should (= 4 (niwa-view-test--composer-point-offset))))))

  :doc "drops the revoked row from the refreshed table"
  (niwa-permissions-list-test--with-buffers
    (setf (niwa-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (with-current-buffer (niwa-permissions-list-test--open context)
      (should (= 2 (length tabulated-list-entries)))
      (niwa-permissions-list-test--row "operation")
      (niwa-test--with-captured-messages nil
        (niwa-permissions-list-revoke))
      (should (= 1 (length tabulated-list-entries)))))

  :doc "removes one stored safe variable and refuses to remove built-in ones"
  (niwa-permissions-list-test--with-buffers
    (let ((rule '("Bash" :pattern "make" :action allow))
          (file (niwa-permission-persistence-file workspace 'workspace)))
      (niwa-permission-persistence-write-store
       file `(:rules (,rule) :resource-grants nil
              :safe-env-vars ("CHECK_BASE" "KEEP_ME")))
      (with-current-buffer (niwa-permissions-list-test--open context)
        (goto-char (point-min))
        (while (not (or (eobp)
                        (equal "CHECK_BASE"
                               (plist-get (niwa-cockpit-surface-selected t)
                                          :spec))))
          (forward-line 1))
        (should-not (eobp))
        (niwa-test--with-captured-messages nil
          (niwa-permissions-list-revoke))
        (goto-char (point-min))
        (while (not (or (eobp)
                        (eq 'built-in
                            (plist-get (niwa-cockpit-surface-selected t)
                                       :scope))))
          (forward-line 1))
        (should-not (eobp))
        (should-error (niwa-permissions-list-revoke) :type 'user-error))
      (let ((authority (niwa-permission-persistent-authority workspace 'workspace)))
        (should (equal '("KEEP_ME") (plist-get authority :safe-env-vars)))
        (should (equal (list rule) (plist-get authority :rules))))))

  :doc "labels exact and recursive grants and revokes exactly the selected scope"
  (niwa-permissions-list-test--with-buffers
    (let ((exact '(:path "/tmp/tree" :access read))
          (recursive '(:path "/tmp/tree" :access read :recursive t)))
      (setf (niwa-session-resource-grants session)
            (list exact recursive))
      (with-current-buffer (niwa-permissions-list-test--open context)
        (goto-char (point-min))
        (should (cl-some (lambda (entry) (string-match-p "(exact)" (aref (cadr entry) 3)))
                         tabulated-list-entries))
        (while (not (or (eobp)
                        (when-let* ((entry (tabulated-list-get-entry)))
                          (string-match-p "(recursive)" (aref entry 3)))))
          (forward-line 1))
        (should-not (eobp))
        (niwa-test--with-captured-messages nil
          (niwa-permissions-list-revoke)))
      (should (equal (list exact)
                     (niwa-session-resource-grants session))))))

(niwa-deftest niwa-permissions-list--help-report ()
  ,test
  (test)
  :doc "documents the keys and what each row kind means"
		 (let ((text (niwa-report-test-text (niwa-permissions-list--help-report))))
		   (should (string-match-p "Permissions help" text))
    (should (string-match-p "d +Revoke the selected authority" text))
    (should (string-match-p "q +Close and return to the previous layout" text))
    (dolist (needle '("operation" "network" "resource"
                      "session" "workspace" "global"))
      (should (string-match-p needle text)))))

(provide 'test-niwa-permissions-list)

;;; test-niwa-permissions-list.el ends here
