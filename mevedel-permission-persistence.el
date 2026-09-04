;;; mevedel-permission-persistence.el -- Persistent permission stores -*- lexical-binding: t -*-

;;; Commentary:

;; Owns permission-store validation, portable path encoding, target-aware
;; reads, and atomic replacement of global and workspace authority files.

;;; Code:

(require 'cl-lib)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-data-buffer
                  "mevedel-permission-mode" ())
(autoload 'mevedel-permission-mode-data-buffer "mevedel-permission-mode")

;; `mevedel-permission-rules'
(declare-function mevedel-permission-rules-build-rule
                  "mevedel-permission-rules"
                  (tool-name action spec-key spec-value &rest keys))
(declare-function mevedel-permission-rules-merge-resource-grant
                  "mevedel-permission-rules"
                  (grants path access &optional recursive))
(declare-function mevedel-permission-rules-resource-grant
                  "mevedel-permission-rules" (path access &optional recursive))
(autoload 'mevedel-permission-rules-build-rule "mevedel-permission-rules")
(autoload 'mevedel-permission-rules-merge-resource-grant
  "mevedel-permission-rules")
(autoload 'mevedel-permission-rules-resource-grant
  "mevedel-permission-rules")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-publish-text
                  "mevedel-session-artifacts"
                  (session path content &optional coding))
(autoload 'mevedel-session-artifacts-publish-text
  "mevedel-session-artifacts")

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-make-directory
                  "mevedel-session-control-fs" (path &optional parents))
(declare-function mevedel-session-control-fs-write-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))
(autoload 'mevedel-session-control-fs-make-directory
  "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-write-file
  "mevedel-session-control-fs")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel-user-dir)

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p
                  "mevedel-transport" (&optional path))
(declare-function mevedel-transport-run-when-idle
                  "mevedel-transport" (key path thunk &optional on-cancel))
(autoload 'mevedel-transport-busy-p "mevedel-transport")
(autoload 'mevedel-transport-run-when-idle "mevedel-transport")

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `mevedel-workspace'
(declare-function mevedel-workspace-state-dir
                  "mevedel-workspace" (workspace))
(autoload 'mevedel-workspace-state-dir "mevedel-workspace")


;;
;;; Persistent rule storage

(defun mevedel-permission-persistence-file (workspace)
  "Return the path to WORKSPACE's persistent permission rules file."
  (file-name-concat (mevedel-workspace-state-dir workspace)
                     "permissions.el"))

(defun mevedel-permission--valid-plist-p (plist allowed required)
  "Return non-nil when PLIST has unique ALLOWED keys including REQUIRED."
  (and (proper-list-p plist)
       (zerop (% (length plist) 2))
       (let ((keys (cl-loop for (key _) on plist by #'cddr collect key)))
         (and (cl-every (lambda (key) (memq key allowed)) keys)
              (= (length keys) (length (delete-dups (copy-sequence keys))))
              (cl-every (lambda (key) (memq key keys)) required)))))

(defun mevedel-permission--normalize-exact-path (path)
  "Expand supported exact PATH syntax, or return nil when invalid."
  (and (stringp path)
       (or (file-name-absolute-p path)
           (equal path "~")
           (string-prefix-p "~/" path))
       (expand-file-name path)))

(defun mevedel-permission--normalize-resource-grant
    (grant &optional path-normalizer)
  "Return normalized resource GRANT, or nil when invalid.
PATH-NORMALIZER defaults to exact-path normalization.  An optional
`:recursive' key must be t or nil; it is preserved only when t."
  (when (and (mevedel-permission--valid-plist-p
              grant '(:path :access :recursive) '(:path :access))
             (memq (plist-get grant :access) '(read write))
             (memq (plist-get grant :recursive) '(t nil)))
    (when-let* ((path (funcall
                       (or path-normalizer
                           #'mevedel-permission--normalize-exact-path)
                       (plist-get grant :path))))
      (append (list :path path :access (plist-get grant :access))
              (and (plist-get grant :recursive) '(:recursive t))))))

(defun mevedel-permission--normalize-rule
    (rule &optional exact-path-normalizer path-pattern-normalizer)
  "Return normalized persistent permission RULE, or nil when invalid.
EXACT-PATH-NORMALIZER normalizes grants, and PATH-PATTERN-NORMALIZER
normalizes rule path patterns."
  (when (and (proper-list-p rule) (stringp (car rule)))
    (let* ((plist (cdr rule))
           (specifier-keys
            (cl-remove-if-not
             (lambda (key) (plist-member plist key))
             '(:path :pattern :domain :name)))
           (file-system-present (plist-member plist :file-system))
           (file-system (plist-get plist :file-system))
           (normalized-file-system
            (and file-system-present
                 (proper-list-p file-system)
                 (mapcar
                  (lambda (grant)
                    (mevedel-permission--normalize-resource-grant
                     grant exact-path-normalizer))
                  file-system))))
      (when (and
             (mevedel-permission--valid-plist-p
              plist
              '(:path :pattern :domain :name :network :file-system
                :sandbox-permissions :action)
              '(:action))
             (<= (length specifier-keys) 1)
             (cl-every (lambda (key) (stringp (plist-get plist key)))
                       specifier-keys)
             (memq (plist-get plist :action) '(allow ask deny))
             (or (not (plist-member plist :network))
                 (eq (plist-get plist :network) t))
             (or (not (plist-member plist :sandbox-permissions))
                 (eq (plist-get plist :sandbox-permissions)
                     'require-escalated))
             (or (not file-system-present)
                 (and (proper-list-p file-system)
                      (cl-every #'identity normalized-file-system))))
        (let* ((normalized (copy-tree rule))
               (path-present (plist-member (cdr normalized) :path))
               (normalized-path
                (and path-present path-pattern-normalizer
                     (funcall path-pattern-normalizer
                              (plist-get (cdr normalized) :path)))))
          (when (or (not (and path-present path-pattern-normalizer))
                    normalized-path)
            (when file-system-present
              (setcdr normalized
                      (plist-put
                       (cdr normalized) :file-system
                       normalized-file-system)))
            (when normalized-path
              (setcdr normalized
                      (plist-put (cdr normalized) :path normalized-path)))
            normalized))))))

(defun mevedel-permission--normalize-store
    (store &optional exact-path-normalizer path-pattern-normalizer)
  "Return normalized permission STORE, or nil when invalid.
EXACT-PATH-NORMALIZER normalizes grants, and PATH-PATTERN-NORMALIZER
normalizes rule path patterns."
  (when (and (mevedel-permission--valid-plist-p
              store '(:rules :resource-grants) '(:rules :resource-grants))
             (proper-list-p (plist-get store :rules))
             (proper-list-p (plist-get store :resource-grants)))
    (let ((rules
           (mapcar
            (lambda (rule)
              (mevedel-permission--normalize-rule
               rule exact-path-normalizer path-pattern-normalizer))
            (plist-get store :rules)))
          (grants
           (mapcar
            (lambda (grant)
              (mevedel-permission--normalize-resource-grant
               grant exact-path-normalizer))
            (plist-get store :resource-grants))))
      (when (and (cl-every #'identity rules)
                 (cl-every #'identity grants))
        (list :rules rules :resource-grants grants)))))

(defun mevedel-permission--home-path-p (path)
  "Return non-nil when PATH is `~' or starts with `~/'."
  (or (equal path "~") (string-prefix-p "~/" path)))

(defun mevedel-permission--abbreviate-home (path home)
  "Return PATH with a leading HOME directory replaced by `~'.
HOME nil leaves PATH unchanged."
  (let* ((home (and (stringp home) (not (string-empty-p home))
                    (directory-file-name home)))
         (prefix (and home (file-name-as-directory home))))
    (cond ((null home) path)
          ((equal path home) "~")
          ((string-prefix-p prefix path)
           (file-name-concat "~" (substring path (length prefix))))
          (t path))))

(defun mevedel-permission--target-home (target)
  "Return TARGET's native home directory, or nil when it is unknown."
  (condition-case nil
      (mevedel-execution-target-native-path
       target (mevedel-execution-target-expand-path target "~"))
    (error nil)))

(defun mevedel-permission--portable-runtime-path
    (path target &optional pattern-p)
  "Return runtime PATH in TARGET's durable path domain.

The durable form is target-native and home-abbreviated: a path under the
target's home directory is stored as `~/...', so the store can be shared
between machines whose home directories differ.  Expansion happens at
load through `mevedel-permission--restore-portable-path'.  PATTERN-P
permits relative path globs.  Remote absolute paths must already carry
TARGET's prefix so client paths cannot become target authority."
  (when (stringp path)
    (condition-case nil
        (cond
         ((file-remote-p path nil 'never)
          (mevedel-permission--abbreviate-home
           (mevedel-execution-target-native-path target path)
           (mevedel-permission--target-home target)))
         ((mevedel-execution-target-remote-p target)
          (cond
           ((mevedel-permission--home-path-p path) path)
           ((and pattern-p (not (file-name-absolute-p path))) path)
           (t nil)))
         ((mevedel-permission--home-path-p path) path)
         ((and pattern-p (not (file-name-absolute-p path))) path)
         ((file-name-absolute-p path)
          (mevedel-permission--abbreviate-home
           (expand-file-name path) (expand-file-name "~"))))
      (mevedel-execution-target-error nil))))

(defun mevedel-permission-serialize-authority (rules grants target)
  "Return RULES and GRANTS encoded in TARGET's durable path domain.

Paths under the target's home are abbreviated to `~/...'.  Return nil when
any entry is malformed or names another filesystem authority."
  (mevedel-permission--normalize-store
   (list :rules rules :resource-grants grants)
   (lambda (path)
     (mevedel-permission--portable-runtime-path path target))
   (lambda (path)
     (mevedel-permission--portable-runtime-path path target t))))

(defun mevedel-permission--restore-portable-path
    (path target &optional pattern-p)
  "Return durable PATH qualified for TARGET.

PATTERN-P permits relative path globs.  Durable authority must not contain a
client-specific remote prefix."
  (when (and (stringp path) (not (file-remote-p path nil 'never)))
    (condition-case nil
        (cond
         ((mevedel-execution-target-remote-p target)
          (cond
           ((or (file-name-absolute-p path)
                (equal path "~") (string-prefix-p "~/" path))
            (mevedel-execution-target-expand-path target path))
           (pattern-p path)))
         (pattern-p path)
         (t (mevedel-permission--normalize-exact-path path)))
      (mevedel-execution-target-error nil))))

(defun mevedel-permission-deserialize-authority (rules grants target)
  "Return durable RULES and GRANTS requalified through TARGET.

Return nil when any entry is malformed or contains a client-specific target."
  (mevedel-permission--normalize-store
   (list :rules rules :resource-grants grants)
   (lambda (path)
     (mevedel-permission--restore-portable-path path target))
   (lambda (path)
     (mevedel-permission--restore-portable-path path target t))))

(defun mevedel-permission--workspace-target (workspace)
  "Return the live execution target for WORKSPACE when available."
  (let* ((data-buffer (mevedel-permission-mode-data-buffer))
         (session (and data-buffer
                       (buffer-local-value 'mevedel--session data-buffer))))
    (if (and session
             (equal (mevedel-workspace-root
                     (mevedel-session-workspace session))
                    (mevedel-workspace-root workspace)))
        (mevedel-session-execution-target session)
      (mevedel-execution-target-create (mevedel-workspace-root workspace)))))

(defvar mevedel-permission--store-cache (make-hash-table :test #'equal)
  "Permission store reads, keyed by (FILE . TARGET-PREFIX).

The tool permission step refreshes both stores once before building its
decision context.  The cache then shares that snapshot across the rule and
resource-grant lookups for the invocation.")

(defun mevedel-permission--store-cache-key (file target)
  "Return the cache key for FILE read against TARGET."
  (cons file (and target (mevedel-execution-target-prefix target))))

(defun mevedel-permission--store-file-status (file &optional target)
  "Return FILE\'s permission store status and normalized contents.
TARGET restores portable target paths when non-nil.  Reads are cached; see
`mevedel-permission--store-cache\'."
  (let ((key (mevedel-permission--store-cache-key file target)))
    (or (gethash key mevedel-permission--store-cache)
        (puthash key
                 (mevedel-permission--read-store-file-uncached file target)
                 mevedel-permission--store-cache))))

(defun mevedel-permission--read-store-file-uncached (file &optional target)
  "Return FILE's permission store status and normalized contents.
TARGET restores portable target paths when non-nil."
  (cond
   ((not (file-exists-p file)) '(:status missing))
   ((not (file-readable-p file))
    '(:status invalid :reason "file is not readable"))
   (t
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (let* ((raw (read (current-buffer)))
                 (store
                  (if (and target
                           (mevedel-permission--valid-plist-p
                            raw '(:rules :resource-grants)
                            '(:rules :resource-grants)))
                      (mevedel-permission-deserialize-authority
                       (plist-get raw :rules)
                       (plist-get raw :resource-grants)
                       target)
                    (mevedel-permission--normalize-store raw)))
                 (single-form-p
                  (condition-case nil
                      (progn (read (current-buffer)) nil)
                    (end-of-file t))))
            (if (and store single-form-p)
                (list :status 'valid :store store)
              '(:status invalid :reason "invalid store shape or value"))))
      (error
       (list :status 'invalid :reason (error-message-string err)))))))

(defun mevedel-permission--read-store-file (file &optional target)
  "Read the permission store plist from FILE, or nil when invalid.
TARGET restores portable target paths when non-nil."
  (let ((result (mevedel-permission--store-file-status file target)))
    (and (eq (plist-get result :status) 'valid)
         (plist-get result :store))))

(defun mevedel-permission-persistence-editable-store (file &optional target)
  "Return FILE's valid store, a new store, or signal on invalid contents.
TARGET restores portable target paths when non-nil."
  (let ((result (mevedel-permission--store-file-status file target)))
    (pcase (plist-get result :status)
      ('valid (plist-get result :store))
      ('missing (list :rules nil :resource-grants nil))
      (_
       (user-error
        "Invalid permission store %s: expected (:rules (...) :resource-grants (...))"
        file)))))

(defun mevedel-permission--store-version (file)
  "Return a warning-cache version for FILE."
  (when-let* ((attributes (file-attributes file)))
    (list (file-attribute-size attributes)
          (file-attribute-modification-time attributes))))

(defun mevedel-permission-validate-persistent-stores (workspace)
  "Warn once per invalid global or WORKSPACE permission store version.

Reads past `mevedel-permission--store-cache\' and replaces its snapshot."
  (let ((target (mevedel-permission--workspace-target workspace)))
    (dolist (entry
             (list (cons (file-name-concat mevedel-user-dir "permissions.el")
                         nil)
                   (cons (mevedel-permission-persistence-file workspace)
                         target)))
      (let* ((file (car entry))
             (result (mevedel-permission--read-store-file-uncached
                      file (cdr entry)))
             (status (plist-get result :status)))
        (puthash (mevedel-permission--store-cache-key file (cdr entry))
                 result mevedel-permission--store-cache)
        (when (eq status 'invalid)
          (mevedel--warn-once
           (list 'permission-store-invalid file
                 (mevedel-permission--store-version file))
           "Invalid permission store %s (%s); expected (:rules (...) :resource-grants (...)). Authority from this file is disabled until fixed"
           file (plist-get result :reason)))))))

(defun mevedel-permission-persistence-refresh
    (workspace continuation &optional on-cancel)
  "Refresh WORKSPACE permission stores, then call CONTINUATION.

Remote refresh waits for an idle transport.  ON-CANCEL runs instead when
queued work is cancelled before it starts."
  (let* ((file (mevedel-permission-persistence-file workspace))
         (refresh
          (lambda ()
            (mevedel-permission-validate-persistent-stores workspace)
            (funcall continuation))))
    (if (mevedel-transport-busy-p file)
        (or (mevedel-transport-run-when-idle
             (list 'permission-store-refresh (gensym))
             file refresh on-cancel)
            (progn
              (when on-cancel (funcall on-cancel))
              nil))
      (funcall refresh)
      t)))

(defun mevedel-permission-persistence-write-store (file store &optional target)
  "Write permission STORE plist to FILE."
  (let* ((store (if target
                    (mevedel-permission-serialize-authority
                     (plist-get store :rules)
                     (plist-get store :resource-grants)
                     target)
                  store))
         (_ (unless store
              (user-error "Permission authority names another execution target")))
         (content
         (with-temp-buffer
           (insert ";; Mevedel persistent permissions\n")
           (insert ";; Auto-generated, safe to edit\n\n")
           (pp store (current-buffer))
           (buffer-string))))
    (if (file-remote-p file)
        (let* ((data-buf (mevedel-permission-mode-data-buffer))
               (session (and data-buf
                             (buffer-local-value 'mevedel--session data-buf))))
          (unless session
            (user-error "Remote permission changes require a live session"))
          (mevedel-session-artifacts-publish-text
           session file content 'utf-8-unix))
      (mevedel-session-control-fs-make-directory
       (directory-file-name (file-name-directory file)) t)
      (mevedel-session-control-fs-write-file file content 'utf-8-unix))
    ;; Cleared wholesale: a write is rare, and a grant must be visible to
    ;; the next permission check whichever file it landed in.
    (clrhash mevedel-permission--store-cache)))

(defun mevedel-permission-persistence-load-rules (workspace)
  "Load persistent permission rules for WORKSPACE.

Loads rules from both the global directory (`mevedel-user-dir') and the
project directory (WORKSPACE's .mevedel/).  Global rules are loaded
first, project rules appended after so they take precedence.

Returns a merged list in `mevedel-permission-rules' format."
  (let ((global-file (file-name-concat mevedel-user-dir "permissions.el"))
        (project-file (mevedel-permission-persistence-file workspace))
        (target (mevedel-permission--workspace-target workspace)))
    (append (plist-get (mevedel-permission--read-store-file global-file) :rules)
            (plist-get (mevedel-permission--read-store-file project-file target)
                       :rules))))

(defun mevedel-permission-persistence-load-resource-grants (workspace)
  "Load resource grants persisted for WORKSPACE."
  (plist-get
   (mevedel-permission--read-store-file
    (mevedel-permission-persistence-file workspace)
    (mevedel-permission--workspace-target workspace))
   :resource-grants))

(defun mevedel-permission-persistent-authority (workspace)
  "Return WORKSPACE's remembered rules and resource grants."
  (or (mevedel-permission--read-store-file
       (mevedel-permission-persistence-file workspace)
       (mevedel-permission--workspace-target workspace))
      '(:rules nil :resource-grants nil)))

(cl-defun mevedel-permission-persistence-save-rule
    (workspace tool-name action &optional path
               &key spec-key spec-value network file-system
               sandbox-permissions)
  "Append a permission rule to WORKSPACE's persistent rules file.

TOOL-NAME and ACTION define the rule.  Positional PATH is equivalent
to SPEC-KEY `:path'.  SPEC-KEY/SPEC-VALUE let callers store rules
qualified by any specifier (`:path', `:pattern', `:domain', `:name').
NETWORK and FILE-SYSTEM record matching additive execution authority.
SANDBOX-PERMISSIONS qualifies an already requested execution level.  The file
is created if it does not exist."
  (let* ((file (mevedel-permission-persistence-file workspace))
         (target (mevedel-permission--workspace-target workspace))
         (store (mevedel-permission-persistence-editable-store file target))
         (existing (plist-get store :rules))
         (key (or spec-key (and path :path)))
         (value (or spec-value path))
         (rule (mevedel-permission-rules-build-rule
                tool-name action key value
                :network network
                :file-system file-system
                :sandbox-permissions sandbox-permissions))
         (updated (if (member rule existing)
                      existing
                    (append existing (list rule)))))
    (mevedel-permission-persistence-write-store
     file (plist-put store :rules updated) target)))

(defun mevedel-permission-persistence-save-resource-grant
    (workspace path access &optional recursive)
  "Persist PATH ACCESS for WORKSPACE.
RECURSIVE non-nil covers PATH and all descendants."
  (let* ((file (mevedel-permission-persistence-file workspace))
         (target (mevedel-permission--workspace-target workspace))
         (store (mevedel-permission-persistence-editable-store file target))
         (grant (mevedel-permission-rules-resource-grant path access recursive))
         (grants
          (mevedel-permission-rules-merge-resource-grant
           (plist-get store :resource-grants) path access recursive)))
    (mevedel-permission-persistence-write-store
     file (plist-put store :resource-grants grants) target)
    grant))

(defun mevedel-permission-remove-persistent-resource-grant
    (workspace path access &optional recursive)
  "Revoke WORKSPACE's PATH ACCESS resource grant with RECURSIVE scope."
  (let* ((file (mevedel-permission-persistence-file workspace))
         (target (mevedel-permission--workspace-target workspace))
         (store (mevedel-permission-persistence-editable-store file target))
         (grant (mevedel-permission-rules-resource-grant path access recursive)))
    (when (file-exists-p file)
      (mevedel-permission-persistence-write-store
       file
       (plist-put store :resource-grants
                  (delete grant
                          (copy-sequence
                           (plist-get store :resource-grants))))
       target))))

(defun mevedel-permission-remove-persistent-rule (workspace rule)
  "Revoke exact permission RULE from WORKSPACE."
  (let* ((file (mevedel-permission-persistence-file workspace))
         (target (mevedel-permission--workspace-target workspace))
         (store (mevedel-permission-persistence-editable-store file target)))
    (when (file-exists-p file)
      (mevedel-permission-persistence-write-store
       file
       (plist-put store :rules
                  (delete rule (copy-sequence (plist-get store :rules))))
       target))))

(provide 'mevedel-permission-persistence)
;;; mevedel-permission-persistence.el ends here
