;;; test-niwa-permission-relative-cockpit.el --- Relative grant selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Check rendered grant identity and revocation through the permissions cockpit.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa)
(require 'niwa-cockpit)
(require 'niwa-permission-persistence)
(require 'niwa-permissions-list)
(require 'niwa-view)

(niwa-deftest niwa-permissions-list-revoke/relative-authority
  (:doc "promotes only bounded authority and revokes its distinct rendered row")
  (let* ((root (make-temp-file "niwa-relative-cockpit-" t))
         (path (file-name-concat root "vault"))
         (workspace (niwa-workspace--create
                     :type 'project :id root :root root :name "relative-cockpit"))
         (session (niwa-session-create "cockpit" workspace root))
         (store (niwa-permission-persistence-file workspace 'workspace))
         (absolute-grant (list :path path :access 'read :recursive t))
         (absolute-label (concat path " (recursive)"))
         (relative-label (concat "./vault [root " (file-name-as-directory root)
                                 "] (recursive)"))
         diagnostics revoked cockpit)
    (unwind-protect
        (niwa-test--with-captured-diagnostics diagnostics
          (make-directory path)
          (make-directory (file-name-directory store) t)
          (with-temp-file store
            (prin1 (list :rules nil :resource-grants
                         (list absolute-grant
                               '(:path "./vault" :access read :recursive t)))
                   (current-buffer)))
          (niwa-view-test--with-buffers
            (with-current-buffer data-buf
              (setq-local niwa--workspace workspace
                          niwa--session session
                          default-directory (file-name-as-directory root)))
            (with-current-buffer view-buf
              (setq-local niwa--session session)
              (setq cockpit
                    (save-window-excursion
                      (niwa-permissions-list-open
                       (niwa-cockpit-current-context)))))
            (with-current-buffer cockpit
              ;; Two grants plus the read-only built-in safe-variable row.
              (should (= 3 (length tabulated-list-entries)))
              (should (string-search absolute-label (buffer-string)))
              (should (string-search relative-label (buffer-string)))
              (goto-char (point-min))
              (search-forward absolute-label)
              (should (equal "read" (aref (tabulated-list-get-entry) 2)))
              (let ((absolute-id (tabulated-list-get-id)))
                (goto-char (point-min))
                (search-forward relative-label)
                (should (equal "read" (aref (tabulated-list-get-entry) 2)))
                (should (tabulated-list-get-id))
                (should-not (equal absolute-id (tabulated-list-get-id))))
              (let* ((selected (niwa-cockpit-surface-selected))
                     (origin (plist-get (plist-get selected :value) :workspace-origin)))
                (should origin)
                (niwa-cockpit-call-in-data
                 (niwa-cockpit-surface-context)
                 #'niwa-permission-persistence-save-resource-grant
                 workspace path 'write t origin))
              (niwa-cockpit-surface-refresh)
              (should (= 3 (length tabulated-list-entries)))
              (goto-char (point-min))
              (search-forward absolute-label)
              (should (equal "read" (aref (tabulated-list-get-entry) 2)))
              (goto-char (point-min))
              (search-forward relative-label)
              (should (equal "write" (aref (tabulated-list-get-entry) 2)))
              (niwa-test--with-captured-diagnostics revoked
                (niwa-permissions-list-revoke))
              (should (string-search
                       (concat "revoked workspace resource: path " relative-label)
                       revoked))
              (should (= 2 (length tabulated-list-entries)))
              (should (string-search absolute-label (buffer-string)))
              (should-not (string-search relative-label (buffer-string)))))
          (let ((remaining (plist-get
                            (niwa-permission-persistent-authority workspace 'workspace)
                            :resource-grants)))
            (should (equal (list absolute-grant) remaining)))
          (with-temp-buffer
            (insert-file-contents store)
            (should (equal (list absolute-grant)
                           (plist-get (read (current-buffer)) :resource-grants)))))
      (when (buffer-live-p cockpit) (kill-buffer cockpit))
      (delete-directory root t))
    (should (string-empty-p diagnostics))))

(provide 'test-niwa-permission-relative-cockpit)
;;; test-niwa-permission-relative-cockpit.el ends here
