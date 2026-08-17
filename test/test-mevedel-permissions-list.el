;;; test-mevedel-permissions-list.el -- Tests for the authority cockpit -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
(require 'mevedel-cockpit)
(require 'mevedel-permissions)
(require 'mevedel-permissions-list)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view)
(require 'mevedel-workspace)

(defmacro mevedel-permissions-list-test--with-buffers (&rest body)
  "Execute BODY with a paired data and view buffer and a live context.
Binds `context' to the cockpit context that owns the surface."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-authority-root-" t))
          (mevedel-user-dir (file-name-as-directory
                             (make-temp-file "mevedel-authority-user-" t)))
          (workspace (mevedel-workspace-get-or-create
                      'project (format "authority-%s" root) root "mevedel"))
          (session (mevedel-session-create "main" workspace))
          (data-buf (generate-new-buffer " *authority-data*"))
          (view-buf (generate-new-buffer " *authority-view*"))
          context)
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (setq-local default-directory (file-name-as-directory root))
             (setq-local mevedel--session session))
           (mevedel-view--setup view-buf data-buf)
           (with-current-buffer view-buf
             (setq context (mevedel-cockpit-current-context)))
           ,@body)
       (when-let* ((buffer (get-buffer mevedel-permissions-list-buffer-name)))
         (kill-buffer buffer))
       (when (buffer-live-p view-buf) (kill-buffer view-buf))
       (when (buffer-live-p data-buf) (kill-buffer data-buf))
       (when (file-directory-p mevedel-user-dir)
         (delete-directory mevedel-user-dir t))
       (when (file-directory-p root)
         (delete-directory root t)))))

(defun mevedel-permissions-list-test--open (context)
  "Open the authority cockpit for CONTEXT and return its buffer."
  (save-window-excursion
    (mevedel-permissions-list-open context)))

(defun mevedel-permissions-list-test--row (kind)
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

(mevedel-deftest mevedel-permissions-list--collect ()
  ,test
  (test)
  :doc "distinguishes session operation, network, and resource authority"
  (mevedel-permissions-list-test--with-buffers
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)
            ("Bash" :pattern "npx test*" :network t :action allow)))
    (setf (mevedel-session-resource-grants session)
          '((:path "/tmp/external" :access read)))
    (let ((items (mevedel-permissions-list--collect context)))
      (should (equal '(operation network resource)
                     (mapcar (lambda (item) (plist-get item :kind)) items)))
      (should (seq-every-p (lambda (item)
                             (eq (plist-get item :scope) 'session))
                           items))))

  :doc "collects workspace authority alongside session authority"
  (mevedel-permissions-list-test--with-buffers
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (mevedel-permission--save-persistent-resource-grant
     workspace "/tmp/workspace-external" 'write)
    (let ((items (mevedel-permissions-list--collect context)))
      (should (equal '(session workspace)
                     (mapcar (lambda (item) (plist-get item :scope)) items)))
      (should (equal 'write (plist-get (nth 1 items) :access))))))

(mevedel-deftest mevedel-permissions-list--header ()
  ,test
  (test)
  :doc "counts remembered authority per scope"
  (mevedel-permissions-list-test--with-buffers
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (mevedel-permission--save-persistent-resource-grant
     workspace "/tmp/workspace-external" 'write)
    (let ((header (substring-no-properties
                   (mevedel-permissions-list--header
                    (mevedel-permissions-list--collect context)
                    context))))
      (should (string-match-p "mevedel: permissions" header))
      (should (string-match-p "2 remembered · 1 session · 1 workspace"
                              header))
      (should (string-match-p "? keys" header))))

  :doc "names the empty state instead of counting nothing"
  (mevedel-permissions-list-test--with-buffers
    (should (string-match-p
             "nothing remembered"
             (substring-no-properties
              (mevedel-permissions-list--header nil context))))))

(mevedel-deftest mevedel-permissions-list-revoke ()
  ,test
  (test)
  :doc "revokes a session network rule without touching its siblings"
  (mevedel-permissions-list-test--with-buffers
    (let ((operation '("Bash" :pattern "npx test*" :action allow))
          (network '("Bash" :pattern "npx test*" :network t :action allow))
          (resource '(:path "/tmp/external" :access read)))
      (setf (mevedel-session-permission-rules session)
            (list operation network))
      (setf (mevedel-session-resource-grants session) (list resource))
      (with-current-buffer (mevedel-permissions-list-test--open context)
        (mevedel-permissions-list-test--row "network")
        (mevedel-test--with-captured-messages nil
          (mevedel-permissions-list-revoke)))
      (should (equal (list operation)
                     (mevedel-session-permission-rules session)))
      (should (equal (list resource)
                     (mevedel-session-resource-grants session)))))

  :doc "revokes one workspace resource without touching workspace rules"
  (mevedel-permissions-list-test--with-buffers
    (let ((network '("Bash" :pattern "npx test*" :network t :action allow)))
      (mevedel-permission--save-persistent-rule
       workspace "Bash" 'allow nil
       :spec-key :pattern :spec-value "npx test*" :network t)
      (mevedel-permission--save-persistent-resource-grant
       workspace "/tmp/workspace-external" 'read)
      (with-current-buffer (mevedel-permissions-list-test--open context)
        (mevedel-permissions-list-test--row "resource")
        (mevedel-test--with-captured-messages nil
          (mevedel-permissions-list-revoke)))
      (let ((authority (mevedel-permission-persistent-authority workspace)))
        (should (equal (list network) (plist-get authority :rules)))
        (should-not (plist-get authority :resource-grants)))))

  :doc "leaves an active composer draft untouched"
  (mevedel-permissions-list-test--with-buffers
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (let ((draft "> quoted\nsecond line"))
      (with-current-buffer view-buf
        (mevedel-view-test--insert-composer-draft draft 4))
      (with-current-buffer (mevedel-permissions-list-test--open context)
        (mevedel-permissions-list-test--row "operation")
        (mevedel-test--with-captured-messages nil
          (mevedel-permissions-list-revoke)))
      (with-current-buffer view-buf
        (should (string= draft (mevedel-view--input-text)))
        (should (= (point) (+ (mevedel-view--input-start) 4))))))

  :doc "drops the revoked row from the refreshed table"
  (mevedel-permissions-list-test--with-buffers
    (setf (mevedel-session-permission-rules session)
          '(("Bash" :pattern "npx test*" :action allow)))
    (with-current-buffer (mevedel-permissions-list-test--open context)
      (should (= 1 (length tabulated-list-entries)))
      (mevedel-permissions-list-test--row "operation")
      (mevedel-test--with-captured-messages nil
        (mevedel-permissions-list-revoke))
      (should-not tabulated-list-entries))))

(mevedel-deftest mevedel-permissions-list--help-text ()
  ,test
  (test)
  :doc "documents the keys and what each row kind means"
  (let ((text (mevedel-permissions-list--help-text)))
    (should (string-match-p "mevedel permissions cockpit" text))
    (should (string-match-p "d +Revoke the selected authority" text))
    (should (string-match-p "q +Back to the main session cockpit" text))
    (dolist (needle '("operation" "network" "resource"
                      "session" "workspace"))
      (should (string-match-p needle text)))))

(provide 'test-mevedel-permissions-list)

;;; test-mevedel-permissions-list.el ends here
