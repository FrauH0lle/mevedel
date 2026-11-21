;;; niwa-permission-persistence.el -- Persistent permission stores -*- lexical-binding: t -*-

;;; Commentary:

;; Owns permission-store validation, portable path encoding, target-aware
;; reads, and atomic replacement of global and workspace authority files.

;;; Code:

(require 'cl-lib)
(require 'niwa-permission-origin)
(require 'niwa-utilities)

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))
(declare-function niwa-execution-target-remote-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-expand-path "niwa-execution-target")
(autoload 'niwa-execution-target-native-path "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-data-buffer
                  "niwa-permission-mode" ())
(autoload 'niwa-permission-mode-data-buffer "niwa-permission-mode")

;; `niwa-permission-rules'
(declare-function niwa-permission-rules-merge-resource-grant
                  "niwa-permission-rules"
                  (grants path access &optional recursive origin))
(declare-function niwa-permission-rules-resource-grant
                  "niwa-permission-rules" (path access &optional recursive origin))
(autoload 'niwa-permission-rules-merge-resource-grant
  "niwa-permission-rules")
(autoload 'niwa-permission-rules-resource-grant
  "niwa-permission-rules")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding))
(autoload 'niwa-session-artifacts-publish-text
  "niwa-session-artifacts")

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-make-directory
                  "niwa-session-control-fs" (path &optional parents))
(declare-function niwa-session-control-fs-write-file
                  "niwa-session-control-fs"
                  (path content &optional coding-system))
(autoload 'niwa-session-control-fs-make-directory
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-write-file
  "niwa-session-control-fs")

;; `niwa-structs'
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-publication-active-p
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa-user-dir)

;; `niwa-transport'
(declare-function niwa-transport-busy-p
                  "niwa-transport" (&optional path))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel))
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-workspace'
(declare-function niwa-workspace-state-dir
                  "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")


;;
;;; Persistent rule storage

(defun niwa-permission-persistence-file (workspace scope)
  "Return WORKSPACE's permission file for SCOPE, `global' or `workspace'.
Remote global stores belong to the target user.  Local global stores
honor `niwa-user-dir'."
  (pcase scope
    ('workspace
     (file-name-concat (niwa-workspace-state-dir workspace)
                       "permissions.el"))
    ('global
     (let ((target (niwa-permission--workspace-target workspace)))
       (if (niwa-execution-target-remote-p target)
           (niwa-execution-target-expand-path
            target "~/.niwa/permissions.el")
         (file-name-concat niwa-user-dir "permissions.el"))))
    (_ (error "Invalid permission scope: %S" scope))))

(defun niwa-permission--valid-plist-p (plist allowed required)
  "Return non-nil when PLIST has unique ALLOWED keys including REQUIRED."
  (and (proper-list-p plist)
       (zerop (% (length plist) 2))
       (let ((keys (cl-loop for (key _) on plist by #'cddr collect key)))
         (and (cl-every (lambda (key) (memq key allowed)) keys)
              (= (length keys) (length (delete-dups (copy-sequence keys))))
              (cl-every (lambda (key) (memq key keys)) required)))))

(defun niwa-permission--normalize-exact-path (path)
  "Expand supported exact PATH syntax, or return nil when invalid."
  (and (stringp path)
       (or (file-name-absolute-p path)
           (equal path "~")
           (string-prefix-p "~/" path))
       (expand-file-name path)))

(defun niwa-permission--normalize-resource-grant
    (grant &optional path-normalizer workspace-root encode)
  "Return normalized resource GRANT, or nil when invalid.
PATH-NORMALIZER defaults to exact-path normalization.  An optional
`:recursive' key must be t or nil; it is preserved only when t.
WORKSPACE-ROOT permits relative grants.  ENCODE writes runtime origins."
  (when (and (niwa-permission--valid-plist-p
              grant (append '(:path :access :recursive)
                            (and workspace-root encode '(:workspace-origin)))
              '(:path :access))
             (memq (plist-get grant :access) '(read write))
             (memq (plist-get grant :recursive) '(t nil)))
    (cond
     ((and workspace-root encode (plist-get grant :workspace-origin))
      (let* ((origin (plist-get grant :workspace-origin))
             (root (plist-get origin :root))
             (relative (plist-get origin :relative)))
        ;; Encoding must preserve unusable configuration for revoke/edit,
        ;; but never serialize a foreign or malformed runtime origin.
        (when (and (stringp root) (stringp relative)
                   (equal relative (niwa-permission-origin-relative relative))
                   (equal root (file-name-as-directory (file-truename workspace-root)))
                   (equal (directory-file-name (plist-get grant :path))
                          (directory-file-name (expand-file-name relative root))))
          (append (list :path relative :access (plist-get grant :access))
                  (and (plist-get grant :recursive) '(:recursive t))))))
     ((and workspace-root (not encode)
           (niwa-permission-origin-relative (plist-get grant :path)))
      (niwa-permission-origin-grant grant workspace-root))
     (t
      (when-let* ((path (funcall
                       (or path-normalizer
                           #'niwa-permission--normalize-exact-path)
                       (plist-get grant :path))))
        (append (list :path path :access (plist-get grant :access))
                (and (plist-get grant :recursive) '(:recursive t))))))))

(defun niwa-permission--normalize-rule
    (rule &optional exact-path-normalizer path-pattern-normalizer workspace-root encode)
  "Return normalized persistent permission RULE, or nil when invalid.
EXACT-PATH-NORMALIZER normalizes grants, and PATH-PATTERN-NORMALIZER
normalizes rule path patterns.  WORKSPACE-ROOT permits relative exact grants;
ENCODE writes runtime origins."
  (when (and (proper-list-p rule) (stringp (car rule)))
    (let* ((plist (cdr rule))
           (specifier-keys
            (cl-remove-if-not
             (lambda (key) (plist-member plist key))
             '(:path :pattern :expression :domain :name)))
           (file-system-present (plist-member plist :file-system))
           (file-system (plist-get plist :file-system))
           (normalized-file-system
            (and file-system-present
                 (proper-list-p file-system)
                 (mapcar
                  (lambda (grant)
                    (niwa-permission--normalize-resource-grant
                     grant exact-path-normalizer workspace-root encode))
                  file-system))))
      (when (and
             (niwa-permission--valid-plist-p
              plist
              '(:path :pattern :expression :domain :name :network :file-system
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

(defun niwa-permission--valid-safe-env-vars-p (names)
  "Return non-nil when NAMES is a list of environment variable names."
  (and (proper-list-p names)
       (cl-every (lambda (name)
                   (and (stringp name)
                        (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)))
                 names)))

(defun niwa-permission--with-safe-env-vars (store names)
  "Return STORE carrying safe environment variable NAMES when non-nil."
  (if names
      (append store (list :safe-env-vars names))
    store))

(defun niwa-permission--normalize-store
    (store &optional exact-path-normalizer path-pattern-normalizer workspace-root encode)
  "Return normalized permission STORE, or nil when invalid.
EXACT-PATH-NORMALIZER normalizes grants, and PATH-PATTERN-NORMALIZER
normalizes rule path patterns.  WORKSPACE-ROOT permits relative exact grants;
ENCODE writes runtime origins.  An optional `:safe-env-vars' list of
variable names is kept as is."
  (when (and (niwa-permission--valid-plist-p
              store '(:rules :resource-grants :safe-env-vars)
              '(:rules :resource-grants))
             (proper-list-p (plist-get store :rules))
             (proper-list-p (plist-get store :resource-grants))
             (niwa-permission--valid-safe-env-vars-p
              (plist-get store :safe-env-vars)))
    (let ((rules
           (mapcar
            (lambda (rule)
              (niwa-permission--normalize-rule
               rule exact-path-normalizer path-pattern-normalizer workspace-root encode))
            (plist-get store :rules)))
          (grants
           (mapcar
            (lambda (grant)
              (niwa-permission--normalize-resource-grant
               grant exact-path-normalizer workspace-root encode))
            (plist-get store :resource-grants))))
      (when (and (cl-every #'identity rules)
                 (cl-every #'identity grants))
        (niwa-permission--with-safe-env-vars
         (list :rules rules :resource-grants grants)
         (plist-get store :safe-env-vars))))))

(defun niwa-permission--home-path-p (path)
  "Return non-nil when PATH is `~' or starts with `~/'."
  (or (equal path "~") (string-prefix-p "~/" path)))

(defun niwa-permission--abbreviate-home (path home)
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

(defun niwa-permission--target-home (target)
  "Return TARGET's native home directory, or nil when it is unknown."
  (condition-case nil
      (niwa-execution-target-native-path
       target (niwa-execution-target-expand-path target "~"))
    (error nil)))

(defun niwa-permission--portable-runtime-path
    (path target &optional pattern-p)
  "Return runtime PATH in TARGET's durable path domain.

The durable form is target-native and home-abbreviated: a path under the
target's home directory is stored as `~/...', so the store can be shared
between machines whose home directories differ.  Expansion happens at
load through `niwa-permission--restore-portable-path'.  PATTERN-P
permits relative path globs.  Remote absolute paths must already carry
TARGET's prefix so client paths cannot become target authority."
  (when (stringp path)
    (condition-case nil
        (cond
         ((file-remote-p path nil 'never)
          (niwa-permission--abbreviate-home
           (niwa-execution-target-native-path target path)
           (niwa-permission--target-home target)))
         ((niwa-execution-target-remote-p target)
          (cond
           ((niwa-permission--home-path-p path) path)
           ((and pattern-p (not (file-name-absolute-p path))) path)
           (t nil)))
         ((niwa-permission--home-path-p path) path)
         ((and pattern-p (not (file-name-absolute-p path))) path)
         ((file-name-absolute-p path)
          (niwa-permission--abbreviate-home
           (expand-file-name path) (expand-file-name "~"))))
      (niwa-execution-target-error nil))))

(defun niwa-permission-serialize-authority (rules grants target &optional workspace-root)
  "Return RULES and GRANTS encoded in TARGET's durable path domain.

Paths under the target's home are abbreviated to `~/...'.  Return nil when
any entry is malformed or names another filesystem authority.
WORKSPACE-ROOT enables workspace-store origin encoding only."
  (niwa-permission--normalize-store
   (list :rules rules :resource-grants grants)
   (lambda (path)
     (niwa-permission--portable-runtime-path path target))
   (lambda (path)
     (niwa-permission--portable-runtime-path path target t))
   workspace-root t))

(defun niwa-permission--restore-portable-path
    (path target &optional pattern-p)
  "Return durable PATH qualified for TARGET.

PATTERN-P permits relative path globs.  Durable authority must not contain a
client-specific remote prefix."
  (when (and (stringp path) (not (file-remote-p path nil 'never)))
    (condition-case nil
        (cond
         ((niwa-execution-target-remote-p target)
          (cond
           ((or (file-name-absolute-p path)
                (equal path "~") (string-prefix-p "~/" path))
            (niwa-execution-target-expand-path target path))
           (pattern-p path)))
         (pattern-p path)
         (t (niwa-permission--normalize-exact-path path)))
      (niwa-execution-target-error nil))))

(defun niwa-permission-deserialize-authority (rules grants target &optional workspace-root)
  "Return durable RULES and GRANTS requalified through TARGET.

Return nil when any entry is malformed or contains a client-specific target.
WORKSPACE-ROOT enables relative exact grants for a workspace store only."
  (niwa-permission--normalize-store
   (list :rules rules :resource-grants grants)
   (lambda (path)
     (niwa-permission--restore-portable-path path target))
   (lambda (path)
     (niwa-permission--restore-portable-path path target t))
   workspace-root))

(defun niwa-permission--workspace-target (workspace)
  "Return the live execution target for WORKSPACE when available."
  (let* ((data-buffer (niwa-permission-mode-data-buffer))
         (session (and data-buffer
                       (buffer-local-value 'niwa--session data-buffer))))
    (if (and session
             (equal (niwa-workspace-root
                     (niwa-session-workspace session))
                    (niwa-workspace-root workspace)))
        (niwa-session-execution-target session)
      (niwa-execution-target-create (niwa-workspace-root workspace)))))

(defvar niwa-permission--store-cache (make-hash-table :test #'equal)
  "Permission store reads, keyed by (FILE . TARGET-PREFIX).

The tool permission step refreshes both stores once before building its
decision context.  The cache then shares that snapshot across the rule and
resource-grant lookups for the invocation.")

(defun niwa-permission--store-cache-key (file target &optional workspace-root)
  "Return the cache key for FILE read against TARGET and WORKSPACE-ROOT."
  (list file (and target (niwa-execution-target-prefix target)) workspace-root))

(defun niwa-permission--store-file-status (file &optional target workspace-root)
  "Return FILE\'s permission store status and normalized contents.
TARGET restores portable target paths when non-nil.  Reads are cached; see
`niwa-permission--store-cache\'.  WORKSPACE-ROOT enables relative grants."
  (let ((key (niwa-permission--store-cache-key file target workspace-root)))
    (or (gethash key niwa-permission--store-cache)
        (puthash key
                 (niwa-permission--read-store-file-uncached file target workspace-root)
                 niwa-permission--store-cache))))

(defun niwa-permission--invalid-grant-reason (grant workspace-root)
  "Explain an invalid durable GRANT for optional WORKSPACE-ROOT."
  (let ((path (and (proper-list-p grant) (plist-get grant :path))))
    (cond
     ((not (niwa-permission--valid-plist-p
            grant '(:path :access :recursive) '(:path :access)))
      "expected :path, :access, and optional :recursive only")
     ((not (memq (plist-get grant :access) '(read write))) "access must be read or write")
     ((not (memq (plist-get grant :recursive) '(t nil))) "recursive must be t or nil")
     ((or (not (stringp path)) (string-empty-p path)) "path must be a nonempty string")
     ((file-remote-p path nil 'never) "stored paths cannot contain a client-specific target prefix")
     ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" path)
      "resource addresses are not filesystem grant paths")
     ((and (not (file-name-absolute-p path)) (member ".." (split-string path "/" t)))
      "parent '..' components are forbidden")
     ((and (not workspace-root) (niwa-permission-origin-relative path))
      "relative exact paths require a workspace store")
     (t "unsupported path or unknown execution-target environment"))))

(defun niwa-permission--invalid-store-reason (raw target workspace-root)
  "Identify the first invalid entry in RAW for TARGET and WORKSPACE-ROOT."
  (let ((exact (and target (lambda (path)
                             (niwa-permission--restore-portable-path path target))))
        (pattern (and target (lambda (path)
                               (niwa-permission--restore-portable-path path target t)))))
    (cl-labels
        ((bad-grant (grants location)
           (if (not (proper-list-p grants))
               (format "%s: grants must be a list" location)
             (cl-loop for grant in grants for index from 1
                      unless (niwa-permission--normalize-resource-grant
                              grant exact workspace-root)
                      return
                      (format "%s grant %d path %S: %s"
                              location index (and (proper-list-p grant)
                                                  (plist-get grant :path))
                              (niwa-permission--invalid-grant-reason grant workspace-root))))))
      (or
       (and (proper-list-p raw)
            (proper-list-p (plist-get raw :rules))
            (cl-loop for rule in (plist-get raw :rules) for index from 1
                     unless (niwa-permission--normalize-rule
                             rule exact pattern workspace-root)
                     return
                     (let ((location (format "rule %d (%S)" index
                                             (and (consp rule) (car rule)))))
                       (or (and (proper-list-p rule)
                                (bad-grant (plist-get (cdr rule) :file-system) location))
                           (format "%s: invalid rule shape or value" location)))))
       (and (proper-list-p raw)
            (bad-grant (plist-get raw :resource-grants) "resource"))
       (and (proper-list-p raw)
            (not (niwa-permission--valid-safe-env-vars-p
                  (plist-get raw :safe-env-vars)))
            ":safe-env-vars must be a list of variable names")
       "invalid store shape or value"))))

(defun niwa-permission--read-store-file-uncached (file &optional target workspace-root)
  "Return FILE's permission store status and normalized contents.
TARGET restores portable target paths when non-nil.
WORKSPACE-ROOT enables relative grants in a workspace store only."
  ;; A missing store is the common case and one existence probe answers it.
  ;; The read reports an unreadable file itself, which saved a remote
  ;; workspace store a readability probe on every refresh.
  (if (not (file-exists-p file))
      '(:status missing)
    (condition-case err
	(with-temp-buffer
          (insert-file-contents file)
          (let* ((raw (read (current-buffer)))
		 (store
                  (if (and target
                           (niwa-permission--valid-plist-p
                            raw '(:rules :resource-grants :safe-env-vars)
                            '(:rules :resource-grants))
                           (niwa-permission--valid-safe-env-vars-p
                            (plist-get raw :safe-env-vars)))
                      (when-let* ((authority
                                   (niwa-permission-deserialize-authority
                                    (plist-get raw :rules)
                                    (plist-get raw :resource-grants)
                                    target workspace-root)))
                        (niwa-permission--with-safe-env-vars
                         authority (plist-get raw :safe-env-vars)))
                    (niwa-permission--normalize-store raw)))
		 (single-form-p
                  (condition-case nil
                      (progn (read (current-buffer)) nil)
                    (end-of-file t))))
            (if (and store single-form-p)
		(list :status 'valid :store store)
              (list :status 'invalid :reason
                    (niwa-permission--invalid-store-reason raw target workspace-root)))))
      (file-missing '(:status missing))
      (permission-denied '(:status invalid :reason "file is not readable"))
      (error
       (list :status 'invalid :reason (error-message-string err))))))

(defun niwa-permission--read-store-file (file &optional target workspace-root)
  "Read the permission store plist from FILE, or nil when invalid.
TARGET restores portable target paths when non-nil.
WORKSPACE-ROOT enables relative grants in a workspace store only."
  (let ((result (niwa-permission--store-file-status file target workspace-root)))
    (and (eq (plist-get result :status) 'valid)
         (plist-get result :store))))

(defun niwa-permission-persistence-editable-store (file &optional target workspace-root)
  "Return FILE's valid store, a new store, or signal on invalid contents.
TARGET restores portable target paths when non-nil.
WORKSPACE-ROOT enables relative grants in a workspace store only."
  (when (niwa-transport-busy-p file)
    (niwa-diagnostic-user-error
     (niwa-diagnostic-scope workspace-root)
     "Execution target is busy; retry the permission change when idle"))
  (let ((result (niwa-permission--read-store-file-uncached file target workspace-root)))
    (pcase (plist-get result :status)
      ('valid (plist-get result :store))
      ('missing (list :rules nil :resource-grants nil))
      (_
       (niwa-diagnostic-user-error
        (niwa-diagnostic-scope workspace-root)
        "Invalid permission store %s: expected (:rules (...) :resource-grants (...))"
        file)))))

(defun niwa-permission--store-version (file)
  "Return a warning-cache version for FILE."
  (when-let* ((attributes (file-attributes file)))
    (list (file-attribute-size attributes)
          (file-attribute-modification-time attributes))))

(defun niwa-permission-validate-persistent-stores (workspace)
  "Warn once per invalid global or WORKSPACE permission store version.

Reads past `niwa-permission--store-cache\' and replaces its snapshot."
  (let ((target (niwa-permission--workspace-target workspace)))
    (dolist (scope '(global workspace))
      (let* ((file (niwa-permission-persistence-file workspace scope))
             (result (niwa-permission--read-store-file-uncached
                      file target (and (eq scope 'workspace)
                                       (niwa-workspace-root workspace))))
             (status (plist-get result :status)))
        (puthash (niwa-permission--store-cache-key
                  file target (and (eq scope 'workspace) (niwa-workspace-root workspace)))
                 result niwa-permission--store-cache)
        (when (eq status 'invalid)
          (niwa--warn-once
           (niwa-diagnostic-scope workspace)
           (list 'permission-store-invalid file
                 (niwa-permission--store-version file))
           "Invalid %s permission store %s (%s); expected (:rules (...) :resource-grants (...)). Authority from this file is disabled until fixed"
           scope file (plist-get result :reason)))))))

(defun niwa-permission-persistence-refresh
    (workspace continuation &optional on-cancel on-error)
  "Refresh WORKSPACE permission stores, then call CONTINUATION.

Remote refresh waits for an idle transport.  ON-CANCEL runs instead when
queued work is cancelled before it starts or its owner buffer dies.
ON-ERROR receives a refresh error instead of leaving deferred work unsettled."
  (let* ((file (niwa-permission-persistence-file workspace 'workspace))
         (buffer (current-buffer))
         (refresh
          (lambda ()
            (if (buffer-live-p buffer)
                (with-current-buffer buffer
                  (when (condition-case err
                            (progn
                              (niwa-permission-validate-persistent-stores workspace)
                              t)
                          (error
                           (if on-error
                               (funcall on-error err)
                             (signal (car err) (cdr err)))
                           nil))
                    (funcall continuation)))
              (when on-cancel (funcall on-cancel))))))
    (if (niwa-transport-busy-p file)
        (or (niwa-transport-run-when-idle
             (list 'permission-store-refresh (gensym))
             file refresh on-cancel)
            (progn
              (when on-cancel (funcall on-cancel))
              nil))
      (funcall refresh)
      t)))

(defun niwa-permission-persistence-write-store (file store &optional target workspace-root)
  "Write permission STORE plist to FILE for TARGET.
WORKSPACE-ROOT enables origin encoding for a workspace store only."
  (let* ((store (if target
                    (when-let* ((authority
                                 (niwa-permission-serialize-authority
                                  (plist-get store :rules)
                                  (plist-get store :resource-grants)
                                  target workspace-root)))
                      (niwa-permission--with-safe-env-vars
                       authority (plist-get store :safe-env-vars)))
                  store))
         (_ (unless store
              (user-error "Permission authority names another execution target")))
         (content
         (with-temp-buffer
           (insert ";; Niwa persistent permissions\n")
           (insert ";; Auto-generated, safe to edit\n\n")
           (pp store (current-buffer))
           (buffer-string))))
    (if (file-remote-p file)
        (let* ((data-buf (niwa-permission-mode-data-buffer))
               (session (and data-buf
                             (buffer-local-value 'niwa--session data-buf))))
          (unless session
            (niwa-diagnostic-user-error
             (niwa-diagnostic-scope workspace-root)
             "Remote permission changes require a live session"))
          (when (niwa-session-publication-active-p session)
            (niwa-diagnostic-user-error
             (niwa-diagnostic-scope (or workspace-root session))
             "Permission changes cannot be queued during publication"))
          (niwa-session-artifacts-publish-text
           session file content 'utf-8-unix))
      (niwa-session-control-fs-make-directory
       (directory-file-name (file-name-directory file)) t)
      (niwa-session-control-fs-write-file file content 'utf-8-unix))
    ;; Cleared wholesale: a write is rare, and a grant must be visible to
    ;; the next permission check whichever file it landed in.
    (clrhash niwa-permission--store-cache)))

(defun niwa-permission-persistence-load-rules (workspace)
  "Load persistent permission rules for WORKSPACE.

Both global and workspace stores belong to the execution target.
Global rules precede workspace rules in the merged persistent bucket;
the ordinary rule matcher determines precedence.

Returns a merged list in `niwa-permission-rules' format."
  (append (plist-get (niwa-permission-persistent-authority workspace 'global)
                    :rules)
          (plist-get (niwa-permission-persistent-authority workspace 'workspace)
                    :rules)))

(defun niwa-permission-persistence-load-resource-grants (workspace)
  "Load global and workspace resource grants for WORKSPACE's target."
  (append (plist-get (niwa-permission-persistent-authority workspace 'global)
                    :resource-grants)
          (plist-get (niwa-permission-persistent-authority workspace 'workspace)
                    :resource-grants)))

(defun niwa-permission-persistence-load-safe-env-vars (workspace)
  "Load the `:safe-env-vars' of WORKSPACE's global and workspace stores."
  (delete-dups
   (append (plist-get (niwa-permission-persistent-authority workspace 'global)
                      :safe-env-vars)
           (plist-get (niwa-permission-persistent-authority workspace 'workspace)
                      :safe-env-vars))))

(defun niwa-permission-persistent-authority (workspace scope)
  "Return WORKSPACE's rules, grants, and safe variables from SCOPE's store."
  (or (niwa-permission--read-store-file
       (niwa-permission-persistence-file workspace scope)
       (niwa-permission--workspace-target workspace)
       (and (eq scope 'workspace) (niwa-workspace-root workspace)))
      '(:rules nil :resource-grants nil)))

(defun niwa-permission-persistence-save-rule (workspace rule)
  "Append RULE to WORKSPACE's persistent rules unless already present.
RULE uses the `niwa-permission-rules' format."
  (let* ((file (niwa-permission-persistence-file workspace 'workspace))
         (target (niwa-permission--workspace-target workspace))
         (root (niwa-workspace-root workspace))
         (store (niwa-permission-persistence-editable-store file target root))
         (existing (plist-get store :rules))
         (updated (if (member rule existing)
                      existing
                    (append existing (list rule)))))
    (niwa-permission-persistence-write-store
     file (plist-put store :rules updated) target root)))

(defun niwa-permission-persistence-save-resource-grant
    (workspace path access &optional recursive origin)
  "Persist PATH ACCESS for WORKSPACE.
RECURSIVE non-nil covers PATH and all descendants.
ORIGIN preserves the identity of an existing bounded relative grant."
  (let* ((file (niwa-permission-persistence-file workspace 'workspace))
         (target (niwa-permission--workspace-target workspace))
         (root (niwa-workspace-root workspace))
         (store (niwa-permission-persistence-editable-store file target root))
         (grant (niwa-permission-rules-resource-grant path access recursive origin))
         (grants
          (niwa-permission-rules-merge-resource-grant
           (plist-get store :resource-grants) path access recursive origin)))
    (niwa-permission-persistence-write-store
     file (plist-put store :resource-grants grants) target root)
    grant))

(defun niwa-permission-remove-persistent-resource-grant
    (workspace scope path access &optional recursive origin)
  "Revoke PATH ACCESS with RECURSIVE extent from WORKSPACE's SCOPE store.
ORIGIN identifies a bounded workspace-relative grant separately."
  (let* ((file (niwa-permission-persistence-file workspace scope))
         (target (niwa-permission--workspace-target workspace))
         (root (and (eq scope 'workspace) (niwa-workspace-root workspace)))
         (store (niwa-permission-persistence-editable-store file target root))
         (grant (niwa-permission-rules-resource-grant path access recursive origin)))
    (when (file-exists-p file)
      (niwa-permission-persistence-write-store
       file
       (plist-put store :resource-grants
                  (delete grant
                          (copy-sequence
                           (plist-get store :resource-grants))))
       target root))))

(defun niwa-permission-remove-persistent-rule (workspace scope rule)
  "Revoke exact permission RULE from WORKSPACE's SCOPE store."
  (let* ((file (niwa-permission-persistence-file workspace scope))
         (target (niwa-permission--workspace-target workspace))
         (root (and (eq scope 'workspace) (niwa-workspace-root workspace)))
         (store (niwa-permission-persistence-editable-store file target root)))
    (when (file-exists-p file)
      (niwa-permission-persistence-write-store
       file
       (plist-put store :rules
                  (delete rule (copy-sequence (plist-get store :rules))))
       target root))))

(defun niwa-permission-remove-persistent-safe-env-var (workspace scope name)
  "Remove safe environment variable NAME from WORKSPACE's SCOPE store."
  (let* ((file (niwa-permission-persistence-file workspace scope))
         (target (niwa-permission--workspace-target workspace))
         (root (and (eq scope 'workspace) (niwa-workspace-root workspace)))
         (store (niwa-permission-persistence-editable-store file target root)))
    (when (file-exists-p file)
      (niwa-permission-persistence-write-store
       file
       (niwa-permission--with-safe-env-vars
        (list :rules (plist-get store :rules)
              :resource-grants (plist-get store :resource-grants))
        (remove name (plist-get store :safe-env-vars)))
       target root))))

(provide 'niwa-permission-persistence)
;;; niwa-permission-persistence.el ends here
