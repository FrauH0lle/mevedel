;;; mevedel-resource.el -- Canonical resource addresses -*- lexical-binding: t -*-

;;; Commentary:

;; The closed address parser and the session-owned directory seam used by the
;; filesystem tools.  This module deliberately keeps physical storage out of
;; model-visible values: callers carry the authored address and use the opaque
;; preparation result only after authorization.

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `cl-lib'
(declare-function cl-find-if "cl-lib" (predicate sequence &rest args))
(declare-function cl-remove-if-not "cl-lib" (predicate sequence &rest args))

;; `mcp'
(declare-function mcp-hub-get-servers "mcp-hub" ())
(declare-function mcp-read-resource "mcp" (connection uri))
(defvar mcp-server-connections)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-list-agents
                  "mevedel-agent-control" (session &optional path-prefix))
(declare-function mevedel-agent-control-settled-result
                  "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-activity
                  "mevedel-agent-control" (record) t)
(declare-function mevedel-agent-record-conversation-buffer
                  "mevedel-agent-control" (record) t)
(declare-function mevedel-agent-record-conversation-location
                  "mevedel-agent-control" (record) t)
(declare-function mevedel-agent-record-path
                  "mevedel-agent-control" (record) t)
(autoload 'mevedel-agent-control-list-agents "mevedel-agent-control")
(autoload 'mevedel-agent-control-settled-result "mevedel-agent-control")
(autoload 'mevedel-agent-record-activity "mevedel-agent-control")
(autoload 'mevedel-agent-record-conversation-buffer "mevedel-agent-control")
(autoload 'mevedel-agent-record-conversation-location "mevedel-agent-control")
(autoload 'mevedel-agent-record-path "mevedel-agent-control")

;; `mevedel-agent-conversation'
(declare-function mevedel-agent-conversation-project-history
                  "mevedel-agent-conversation" (buffer &optional session))
(autoload 'mevedel-agent-conversation-project-history
  "mevedel-agent-conversation")

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-restore-tree
                  "mevedel-agent-persistence"
                  (session root-buffer &optional readonly-p))
(autoload 'mevedel-agent-persistence-restore-tree
  "mevedel-agent-persistence")

;; `mevedel-execution'
(declare-function mevedel-execution-list-user "mevedel-execution" (session))

;; `mevedel-skills-core'
(declare-function mevedel-skill-description "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-name "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source-dir "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (skill) t)
(declare-function mevedel-skills-skill-enabled-p "mevedel-skills-core" (skill))
(declare-function mevedel-skills-source-key "mevedel-skills-core" (source-file))
(declare-function mevedel-skills-scan "mevedel-skills-core"
                  (&optional workspace-root dirs workspace))
(autoload 'mevedel-skills-scan "mevedel-skills-core")

;; `mevedel-structs'
(declare-function mevedel-agent-path-p "mevedel-structs" (path))
(declare-function mevedel-session-agent-registry "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-system'
(declare-function mevedel-system--memory-content "mevedel-system"
                  (&optional workspace))
(declare-function mevedel-system--memory-roots "mevedel-system"
                  (&optional workspace))
(autoload 'mevedel-system--memory-content "mevedel-system")
(autoload 'mevedel-system--memory-roots "mevedel-system")

;; `mevedel-utilities'
(declare-function mevedel--transcript-org-mode "mevedel-utilities" ())
(autoload 'mevedel--transcript-org-mode "mevedel-utilities")

(defconst mevedel-resource-supported-schemes
  '(local artifact skill agent history memory mcp)
  "Closed set of resource address schemes understood by mevedel.")

(defconst mevedel-resource--unreserved
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
  "RFC 3986 unreserved bytes.")

(defvar mevedel-resource-current-attempts nil
  "Dynamically bound resource attempts for the active pipeline handler.
The pipeline binds this around each handler; it is the module's
dynamic seam, not private state.")

(defvar mevedel-resource--attempt-table (make-hash-table :test #'eq)
  "Private table mapping opaque attempt tokens to resolver data.")

(defvar mevedel-resource-attempts-cell nil
  "Dynamically bound cell collecting attempts for one pipeline run.")

(define-error 'mevedel-resource-error "Resource address error")
(define-error 'mevedel-resource-unavailable "Resource unavailable")

(defun mevedel-resource--control-character-p (character)
  "Return non-nil when CHARACTER is a disallowed control character."
  (or (< character #x20) (= character #x7f)))

(defun mevedel-resource--has-control-character-p (value)
  "Return non-nil when VALUE contains a disallowed control character."
  (let ((index 0)
        found)
    (while (and (< index (length value)) (not found))
      (setq found (mevedel-resource--control-character-p
                   (aref value index)))
      (setq index (1+ index)))
    found))

(defun mevedel-resource--lowercase-digest-p (value)
  "Return non-nil when VALUE is a lowercase SHA-256 digest."
  (let ((case-fold-search nil))
    (and (stringp value)
         (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value))))

(defun mevedel-resource-supported-scheme-p (scheme)
  "Return the scheme symbol SCHEME names, or nil.
SCHEME is a name or a symbol; a name is looked up, never interned."
  (let ((symbol (if (stringp scheme)
                    (intern-soft (downcase scheme))
                  scheme)))
    (and (memq symbol mevedel-resource-supported-schemes) symbol)))

(defun mevedel-resource--scheme-prefix (value)
  "Return the scheme name before `://` in VALUE, or nil.
Any URI-shaped prefix answers, not only a supported one: that is what
makes an unknown scheme a rejected address rather than a relative
filesystem path."
  (when (and (stringp value)
             (string-match "\\`\\([[:alnum:]][[:alnum:]+.-]*\\)://" value))
    (downcase (match-string 1 value))))

(defun mevedel-resource-address-like-p (value)
  "Return non-nil when VALUE has a URI-like `scheme://` prefix."
  (and (stringp value) (mevedel-resource--scheme-prefix value)))

(defun mevedel-resource-address-p (value)
  "Return non-nil when VALUE starts with a supported resource prefix."
  (and (stringp value)
       (mevedel-resource-supported-scheme-p
        (mevedel-resource--scheme-prefix value))))

(defun mevedel-resource-normalize-file-path (value &optional directory)
  "Return ordinary filesystem VALUE as an absolute locator.

Environment references are substituted before expansion.  DIRECTORY is the
base directory for relative values; malformed substitutions leave VALUE
literal and are expanded in that same base directory."
  (if (or (not (stringp value)) (string-empty-p value))
      value
    (expand-file-name
     (condition-case nil
         (substitute-in-file-name value)
       (error value))
     directory)))

(defun mevedel-resource-encode-component (value)
  "Return VALUE encoded as one canonical RFC 3986 UTF-8 component."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (let ((bytes (encode-coding-string value 'utf-8 t))
        (result nil))
    (dotimes (index (length bytes))
      (let ((byte (aref bytes index)))
        (if (and (< byte 128)
                 (string-match-p
                  (regexp-quote (char-to-string byte))
                  mevedel-resource--unreserved))
            (push (char-to-string byte) result)
          (push (format "%%%02X" byte) result))))
    (apply #'concat (nreverse result))))

(defun mevedel-resource--decode-component (raw &optional allow-separator)
  "Decode one RAW component and reject malformed or noncanonical escapes.

When ALLOW-SEPARATOR is non-nil, a decoded slash is data within the
component.  This is used only for the encoded native URI component of an
MCP address."
  (let ((index 0)
        (bytes nil))
    (while (< index (length raw))
      (let ((char (aref raw index)))
        (if (= char ?%)
            (progn
              (when (or (> (+ index 2) (1- (length raw)))
                        (not (and (string-match-p
                                   "\\`[0-9A-Fa-f]\\'"
                                   (char-to-string (aref raw (1+ index))))
                                  (string-match-p
                                   "\\`[0-9A-Fa-f]\\'"
                                   (char-to-string (aref raw (+ index 2)))))))
                (signal 'mevedel-resource-error
                        (list "Malformed percent escape in resource address")))
              (push (string-to-number (substring raw (1+ index) (+ index 3))
                                      16)
                    bytes)
              (setq index (+ index 3)))
          (when (>= char 128)
            (signal 'mevedel-resource-error
                    (list "Non-ASCII bytes must be percent-encoded")))
          (push char bytes)
          (setq index (1+ index)))))
    (let* ((decoded
            (decode-coding-string
             (apply #'unibyte-string (nreverse bytes)) 'utf-8 t))
           (canonical (mevedel-resource-encode-component decoded)))
      (when (or (string-empty-p decoded)
                (mevedel-resource--has-control-character-p decoded)
                (and (not allow-separator)
                     (member decoded '("." "..")))
                (and (not allow-separator) (string-match-p "/" decoded))
                (not (equal raw canonical)))
        (signal 'mevedel-resource-error
                (list "Noncanonical or unsafe resource path component")))
      decoded)))

(defun mevedel-resource--parse-components (tail)
  "Parse slash-separated path TAIL into decoded canonical components."
  (if (string-empty-p tail)
      nil
    (let ((raw-components (split-string tail "/" nil)))
      (when (or (member "" raw-components)
                (string-prefix-p "/" tail)
                (string-suffix-p "/" tail))
        (signal 'mevedel-resource-error
                (list "Empty resource path component")))
      (mapcar #'mevedel-resource--decode-component raw-components))))

(defun mevedel-resource--canonical-components (components)
  "Return canonical slash-separated encoding for COMPONENTS."
  (if components
      (mapconcat #'mevedel-resource-encode-component components "/")
    ""))

(defun mevedel-resource--decode-json-pointer (fragment)
  "Validate decoded JSON Pointer FRAGMENT and return its tokens.

FRAGMENT is the decoded URI fragment, not the raw address spelling.  The
empty fragment selects the complete JSON value."
  (when (mevedel-resource--has-control-character-p fragment)
    (signal 'mevedel-resource-error
            (list "JSON Pointer fragment contains a control character")))
  (unless (or (string-empty-p fragment)
              (string-prefix-p "/" fragment))
    (signal 'mevedel-resource-error
            (list "JSON Pointer fragment must begin with '/'")))
  (if (string-empty-p fragment)
      nil
    (mapcar
     (lambda (token)
       (let ((index 0)
             (decoded nil))
         (while (< index (length token))
           (let ((character (aref token index)))
             (if (= character ?~)
                 (progn
                   (when (or (= (1+ index) (length token))
                             (not (memq (aref token (1+ index)) '(?0 ?1))))
                     (signal 'mevedel-resource-error
                             (list "Invalid JSON Pointer escape")))
                   (push (if (= (aref token (1+ index)) ?0) ?~ ?/)
                         decoded)
                   (setq index (+ index 2)))
               (push character decoded)
               (setq index (1+ index)))))
         (apply #'string (nreverse decoded))))
     (split-string (substring fragment 1) "/" nil))))

(defun mevedel-resource--decode-fragment (raw)
  "Decode and canonicalize an agent JSON Pointer RAW fragment.

Return a plist containing decoded `:fragment', canonical `:raw', and pointer
`:tokens'.  URI percent decoding precedes RFC 6901 token decoding."
  (let ((index 0)
        (bytes nil))
    (while (< index (length raw))
      (let ((character (aref raw index)))
        (if (= character ?%)
            (progn
              (when (or (> (+ index 2) (1- (length raw)))
                        (not (and (string-match-p
                                   "\\`[0-9A-Fa-f]\\'"
                                   (char-to-string (aref raw (1+ index))))
                                  (string-match-p
                                   "\\`[0-9A-Fa-f]\\'"
                                   (char-to-string (aref raw (+ index 2)))))))
                (signal 'mevedel-resource-error
                        (list "Malformed percent escape in JSON Pointer")))
              (push (string-to-number (substring raw (1+ index) (+ index 3))
                                      16)
                    bytes)
              (setq index (+ index 3)))
          (when (or (>= character 128) (= character ??))
            (signal 'mevedel-resource-error
                    (list "JSON Pointer fragment must be canonically encoded")))
          (push character bytes)
          (setq index (1+ index)))))
    (let* ((fragment
            (decode-coding-string
             (apply #'unibyte-string (nreverse bytes)) 'utf-8 t))
           (tokens (mevedel-resource--decode-json-pointer fragment))
           (canonical
            (mapconcat
             (lambda (character)
               (if (or (= character ?/) (= character ?~)
                       (and (< character 128)
                            (string-match-p
                             (regexp-quote (char-to-string character))
                             mevedel-resource--unreserved)))
                   (char-to-string character)
                 (mevedel-resource-encode-component
                  (char-to-string character))))
             (string-to-list fragment) "")))
      (unless (equal raw canonical)
        (signal 'mevedel-resource-error
                (list "Noncanonical JSON Pointer fragment")))
      (list :fragment fragment :raw canonical :tokens tokens))))

(defun mevedel-resource--parse-skill-tail (tail)
  "Parse the skill-specific TAIL and return its locator fields."
  (if (string-empty-p tail)
      (list :components nil :name nil :source-key nil :dynamic-p t)
    (let* ((slash (string-match "/" tail))
           (head (if slash (substring tail 0 slash) tail))
           (path (and slash (substring tail (1+ slash))))
           (at (string-match "@" head)))
      (when (or (null at)
                (= at 0)
                (= (1+ at) (length head))
                (string-match "@" (substring head (1+ at)))
                (not (mevedel-resource--lowercase-digest-p
                      (substring head (1+ at)))))
        (signal 'mevedel-resource-error
                (list "Skill address requires a name and lowercase source digest")))
      (let* ((name (mevedel-resource--decode-component
                    (substring head 0 at)))
             (source-key (substring head (1+ at)))
             (components (if slash
                            (mevedel-resource--parse-components path)
                          nil)))
        (list :components components :name name :source-key source-key
              :dynamic-p nil)))))

(defun mevedel-resource--parse-memory-tail (tail)
  "Parse the memory-specific TAIL and return its locator fields."
  (let ((components (mevedel-resource--parse-components tail)))
    (cond
     ((equal components '("root"))
      (list :components components :dynamic-p t))
     ((and (>= (length components) 2)
           (mevedel-resource--lowercase-digest-p (car components)))
      (list :components components :dynamic-p nil))
     (t
      (signal 'mevedel-resource-error
              (list "Memory address requires 'root' or a root digest and path"))))))

(defun mevedel-resource--parse-agent-history-tail (tail)
  "Parse canonical retained-agent TAIL for agent or history resources."
  (if (string-empty-p tail)
      (list :components nil :dynamic-p t)
    (let ((components (mevedel-resource--parse-components tail)))
      (unless (and (> (length components) 1)
                   (equal (car components) "root"))
        (signal 'mevedel-resource-error
                (list "Agent and history addresses require a retained /root path")))
      (unless (mevedel-agent-path-p
               (concat "/" (string-join components "/")))
        (signal 'mevedel-resource-error
                (list "Agent and history addresses require a canonical agent path")))
      (list :components components :dynamic-p nil))))

(defun mevedel-resource--parse-mcp-tail (tail)
  "Parse the MCP-specific TAIL and return its locator fields."
  (if (string-empty-p tail)
      (list :components nil :dynamic-p t)
    (let ((raw-components (split-string tail "/" nil)))
      (when (or (member "" raw-components) (> (length raw-components) 2))
        (signal 'mevedel-resource-error
                (list "MCP address has an invalid component count")))
      (let ((components
             (list (mevedel-resource--decode-component
                    (car raw-components) t)))
            (resource-p (> (length raw-components) 1)))
        (when resource-p
          (setq components
                (append components
                        (list (mevedel-resource--decode-component
                               (cadr raw-components) t)))))
        (list :components components :dynamic-p (not resource-p))))))

(defun mevedel-resource--session (context)
  "Return the owning session from CONTEXT, or nil."
  (or (plist-get context :session)
      (and (boundp 'mevedel--session) mevedel--session)))

(defun mevedel-resource--workspace (context session)
  "Return the workspace represented by CONTEXT and SESSION."
  (or (plist-get context :workspace)
      (and session (mevedel-session-workspace session))))

(defun mevedel-resource--digest (value)
  "Return the lowercase SHA-256 digest of canonical locator VALUE."
  (secure-hash 'sha256 value))

(defun mevedel-resource--skill-source-key (source-file)
  "Return the canonical skill source key for SOURCE-FILE."
  (or (and (fboundp 'mevedel-skills-source-key)
           (mevedel-skills-source-key source-file))
      (concat "file:" (file-truename source-file))))

(defun mevedel-resource-skill-digest (source-file)
  "Return the lowercase address digest for skill SOURCE-FILE."
  (mevedel-resource--digest
   (mevedel-resource--skill-source-key source-file)))

(defun mevedel-resource--memory-root-key (root)
  "Return the stable digest key for memory ROOT metadata."
  (mevedel-resource--digest
   (concat "memory:" (file-name-as-directory
                       (file-truename (plist-get root :dir))))))

(defun mevedel-resource-memory-root-key (root)
  "Return the stable address key for memory ROOT metadata or directory."
  (mevedel-resource--memory-root-key
   (if (listp root)
       root
     (list :dir root))))

(defun mevedel-resource--skill-list (session context)
  "Return currently discoverable skills for SESSION and CONTEXT."
  (let* ((workspace (mevedel-resource--workspace context session))
         (workspace-root (and workspace (mevedel-workspace-root workspace)))
         (skills (and session (mevedel-session-skills session))))
    (or skills
        (mevedel-skills-scan workspace-root nil workspace))))

(defun mevedel-resource--skill-for-digest (digest session context)
  "Return the skill whose exact source identity hashes to DIGEST."
  (cl-find-if
   (lambda (skill)
     (let ((source (ignore-errors
                     (mevedel-skill-source-file skill))))
       (and source
            (equal digest (mevedel-resource-skill-digest source))
            (or (not (fboundp 'mevedel-skills-skill-enabled-p))
                (mevedel-skills-skill-enabled-p skill)))))
   (mevedel-resource--skill-list session context)))

(defun mevedel-resource--skill-root (skill)
  "Return SKILL's package root, deriving it from its source when needed."
  (or (mevedel-skill-source-dir skill)
      (when-let ((source (mevedel-skill-source-file skill)))
        (file-name-directory source))))

(defun mevedel-resource--skill-address (skill)
  "Return the canonical address for SKILL's exact source."
  (format "skill://%s@%s"
          (mevedel-resource-encode-component
           (mevedel-skill-name skill))
          (mevedel-resource-skill-digest
           (mevedel-skill-source-file skill))))

(defun mevedel-resource--memory-roots (context session)
  "Return configured memory root metadata for CONTEXT and SESSION."
  (mevedel-system--memory-roots
   (mevedel-resource--workspace context session)))

(defun mevedel-resource-completion-metadata (context &optional scheme)
  "Return resource-owned metadata for completion in CONTEXT and SCHEME.

The returned plist keeps scheme-specific lookup and path safety inside this
module.  Function-valued slots are intentionally opaque operations for the
completion consumer; they do not expose backing roots as candidates.  When
SCHEME is nil, include metadata for every scheme."
  (let* ((session (mevedel-resource--session context))
         (skills (and (memq scheme '(nil skill))
                      session
                      (cl-remove-if-not
                       (lambda (skill)
                         (let ((source (mevedel-skill-source-file skill)))
                           (and source
                                (or (null scheme)
                                    (not (file-remote-p source)))
                                (or (not (fboundp
                                          'mevedel-skills-skill-enabled-p))
                                    (mevedel-skills-skill-enabled-p skill)))))
                       (if scheme
                           (mevedel-session-skills session)
                         (mevedel-resource--skill-list session context)))))
         (agents (and (memq scheme '(nil agent history))
                      session
                      (mevedel-agent-control-list-agents session)))
         (memory-roots (and (memq scheme '(nil memory))
                            session
                            (mevedel-resource--workspace context session)
                            (mevedel-resource--memory-roots context session)))
         (servers (and (memq scheme '(nil mcp))
                       (fboundp 'mcp-hub-get-servers)
                       (condition-case nil
                           (mcp-hub-get-servers)
                         (error nil)))))
    (list :roots
          (delq nil
                (list
                 (and (memq scheme '(nil local))
                      (cons 'local
                            (mevedel-resource--root 'local session)))
                 (and (memq scheme '(nil artifact))
                      (cons 'artifact
                            (mevedel-resource--root 'artifact session)))))
          :decode-component #'mevedel-resource--decode-component
          :safe-path #'mevedel-resource--safe-path
          :skills
          (delq nil
                (mapcar
                 (lambda (skill)
                   (when-let ((address
                               (condition-case nil
                                   (mevedel-resource--skill-address skill)
                                 (error nil))))
                     (list :skill skill :address address)))
                 skills))
          :agents
          (mapcar
           (lambda (item)
             (list :item item
                   :record
                   (mevedel-resource--agent-record
                    (plist-get item :path) session)))
           agents)
          :memory-roots
          (delq nil
                (mapcar (lambda (root)
                          (when (or (null scheme)
                                    (not (file-remote-p
                                          (plist-get root :dir))))
                            (list
                             :root root
                             :key (mevedel-resource--memory-root-key root))))
                        memory-roots))
          :mcp-servers servers)))

(defun mevedel-resource--memory-root-for-key (key context session)
  "Return the configured memory root whose digest is KEY."
  (cl-find-if
   (lambda (root)
     (equal key (mevedel-resource--memory-root-key root)))
   (mevedel-resource--memory-roots context session)))

(defun mevedel-resource--canonical-relative (path root)
  "Return PATH relative to ROOT with canonical slash separators."
  (mapconcat #'identity
             (file-name-split (file-relative-name path root))
             "/"))

(defun mevedel-resource--safe-path (root components)
  "Return a contained path below ROOT for decoded COMPONENTS.

The lexical parser rejects traversal.  This second check rejects symlink
escapes and is deliberately performed during preparation, before a handler
or permission callback sees a target."
  (when root
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (path (expand-file-name
                  (mapconcat #'identity components "/") root)))
      (unless (mevedel-resource-within-root-p path root)
        (signal 'mevedel-resource-error
                (list "Resource address escapes its owning root")))
      path)))

(defun mevedel-resource--file-list (root &optional scheme)
  "Return regular files beneath ROOT in deterministic relative order.

Directory traversal never follows symlinks.  Paths whose final target is
outside ROOT are discarded as well, covering symlinked files reported by the
directory walker.  Pending execution spools are private until execution
yield makes them public under `executions/'."
  (let ((root (and root (file-name-as-directory (expand-file-name root)))))
    (if (or (null root)
            (not (file-directory-p root))
            (file-symlink-p (directory-file-name root)))
        nil
      (sort
       (delq nil
             (mapcar
              (lambda (path)
                (when (and (file-regular-p path)
                           (mevedel-resource-within-root-p path root))
                  (let ((relative (mevedel-resource--canonical-relative
                                   path root)))
                    (unless (and (eq scheme 'artifact)
                                 (string-match-p
                                  "\\`\\.mevedel-pending-executions\\(?:/\\|\\'\\)"
                                  relative))
                      relative))))
              (directory-files-recursively root "." nil nil nil)))
       #'string-lessp))))

(defun mevedel-resource--logical-address (scheme components)
  "Return canonical SCHEME address for decoded COMPONENTS."
  (concat (symbol-name scheme) "://"
          (mevedel-resource--canonical-components components)))

(defun mevedel-resource--directory-list-result (scheme root)
  "Return a logical listing for directory-backed SCHEME ROOT."
  (let ((entries (mevedel-resource--file-list root scheme)))
    (if entries
        (string-join
         (mapcar (lambda (entry)
                   (mevedel-resource--logical-address
                    scheme (split-string entry "/" t)))
                 entries)
         "\n")
      (format "No files found under %s://" (symbol-name scheme)))))

(defun mevedel-resource-parse-address (address)
  "Parse canonical ADDRESS and return a locator plist.

The plist contains decoded `:components', canonical `:canonical', and
`:locator-class' (`exact', `session-relative', or `dynamic').  Physical
resolution is intentionally not performed here."
  (unless (stringp address)
    (signal 'mevedel-resource-error (list "Resource address must be text")))
  (when (string-match-p "\\?" address)
    (signal 'mevedel-resource-error
            (list "Resource addresses do not support query strings")))
  (let* ((name (mevedel-resource--scheme-prefix address))
         (scheme (and name (mevedel-resource-supported-scheme-p name))))
    (unless name
      (signal 'mevedel-resource-error
              (list "Resource address must use a supported scheme")))
    (unless scheme
      (signal 'mevedel-resource-error
              (list (format "Unsupported resource scheme: %s" name))))
    (let* ((prefix (concat name "://"))
           (tail (substring address (length prefix)))
           (fragment-data nil)
           (fragment-p (and (eq scheme 'agent)
                            (string-match "#" tail))))
      (when fragment-p
        (let ((hash (string-match "#" tail)))
          (setq fragment-data
                (mevedel-resource--decode-fragment (substring tail (1+ hash))))
          (setq tail (substring tail 0 hash))))
      (when (and (not (eq scheme 'agent)) (string-match "#" tail))
        (signal 'mevedel-resource-error
                (list "Fragments are not supported by this resource scheme")))
      (let* ((specific
              (pcase scheme
                ('skill (mevedel-resource--parse-skill-tail tail))
                ((or 'agent 'history)
                 (mevedel-resource--parse-agent-history-tail tail))
                ('memory (mevedel-resource--parse-memory-tail tail))
                ('mcp (mevedel-resource--parse-mcp-tail tail))
                (_ (list :components (mevedel-resource--parse-components tail)
                         :dynamic-p (string-empty-p tail)))))
             (components (plist-get specific :components))
             (fragment (plist-get fragment-data :fragment))
             (canonical-tail
              (cond
               ((eq scheme 'skill)
                (if (plist-get specific :name)
                    (concat (mevedel-resource-encode-component
                             (plist-get specific :name)) "@"
                            (plist-get specific :source-key)
                            (if components
                                (concat "/"
                                        (mevedel-resource--canonical-components
                                         components))
                              ""))
                  ""))
               ((eq scheme 'mcp)
                (mevedel-resource--canonical-components components))
               (t (mevedel-resource--canonical-components components))))
             (canonical (concat prefix canonical-tail
                                (if fragment-p
                                    (concat "#" (plist-get fragment-data :raw))
                                  "")))
             (class
              (cond
               ((plist-get specific :dynamic-p) 'dynamic)
               ((memq scheme '(local artifact agent history))
                'session-relative)
               (t 'exact))))
        (when (and (eq scheme 'agent)
                   fragment-p
                   (null components))
          (signal 'mevedel-resource-error
                  (list "Agent JSON Pointer requires a canonical agent path")))
        (when (and (eq scheme 'agent)
                   (not fragment-p)
                   (string-match "#" address))
          (signal 'mevedel-resource-error
                  (list "Agent address contains an invalid fragment")))
        (unless (equal address canonical)
          (signal 'mevedel-resource-error
                  (list "Noncanonical resource address")))
        (list :scheme scheme
              :components components
              :name (plist-get specific :name)
              :source-key (plist-get specific :source-key)
              :fragment fragment
              :fragment-p fragment-p
              :pointer (and fragment-data
                            (plist-get fragment-data :tokens))
              :canonical canonical
              :locator-class class
              :dynamic-p (eq class 'dynamic))))))

(defun mevedel-resource--root (scheme session)
  "Return the physical root for SCHEME and SESSION, without creating it."
  (let ((save-path (and session (mevedel-session-save-path session))))
    (and save-path
         (pcase scheme
           ('local (file-name-concat save-path "local"))
           ('artifact (file-name-concat save-path "tool-results"))))))

(defun mevedel-resource--mcp-servers ()
  "Return current MCP server metadata, or signal when mcp.el is absent."
  (unless (fboundp 'mcp-hub-get-servers)
    (signal 'mevedel-resource-unavailable
            (list "MCP support is unavailable")))
  (or (mcp-hub-get-servers) nil))

(defun mevedel-resource--mcp-server (name)
  "Return current MCP metadata for server NAME, or nil."
  (cl-find-if (lambda (server)
                (equal (plist-get server :name) name))
              (mevedel-resource--mcp-servers)))

(defun mevedel-resource--mcp-connection (name)
  "Return the current MCP connection for NAME, or nil."
  (and (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (gethash name mcp-server-connections)))

(defun mevedel-resource--mcp-address (server uri)
  "Return the canonical MCP address for SERVER and native URI URI."
  (concat "mcp://"
          (mevedel-resource-encode-component server)
          "/"
          (mevedel-resource-encode-component uri)))

(defun mevedel-resource-mcp-extract-text (response)
  "Return concatenated text content from an MCP resource RESPONSE."
  (let ((contents (plist-get response :contents)))
    (string-join
     (delq nil
           (mapcar (lambda (content)
                     (let ((text (plist-get content :text)))
                       (and (stringp text) text)))
                   (if (vectorp contents)
                       (append contents nil)
                     contents)))
     "\n")))

(defun mevedel-resource--mcp-list-result (&optional server-info)
  "Return a model-visible listing for SERVER-INFO or all MCP servers."
  (if (null server-info)
      (let ((servers (sort (copy-sequence (mevedel-resource--mcp-servers))
                           (lambda (left right)
                             (string-lessp (or (plist-get left :name) "")
                                           (or (plist-get right :name) ""))))))
        (if servers
            (string-join
             (mapcar (lambda (server)
                       (format "mcp://%s\t%s"
                               (mevedel-resource-encode-component
                                (plist-get server :name))
                               (or (plist-get server :status) "unknown")))
                     servers)
             "\n")
          "No MCP servers configured"))
    (let* ((server (plist-get server-info :name))
           (resources (plist-get server-info :resources))
           (resources (if (vectorp resources)
                          (append resources nil)
                        resources)))
      (if (not (eq 'connected (plist-get server-info :status)))
          (signal 'mevedel-resource-unavailable
                  (list (format "MCP server %s is not connected" server)))
        (if resources
            (string-join
             (mapcar (lambda (resource)
                       (format "%s\t%s"
                               (mevedel-resource--mcp-address
                                server (plist-get resource :uri))
                               (or (plist-get resource :name)
                                   (plist-get resource :description)
                                   "")))
                     resources)
             "\n")
          (format "No MCP resources advertised by %s" server))))))

(defun mevedel-resource--mcp-read (server uri)
  "Read URI from connected MCP SERVER using the mention contract."
  (let ((server-info (mevedel-resource--mcp-server server)))
    (unless server-info
      (signal 'mevedel-resource-unavailable
              (list (format "Unknown MCP server: %s" server))))
    (unless (eq 'connected (plist-get server-info :status))
      (signal 'mevedel-resource-unavailable
              (list (format "MCP server %s is not connected" server))))
    (let ((connection (mevedel-resource--mcp-connection server)))
      (unless connection
        (signal 'mevedel-resource-unavailable
                (list (format "No active MCP connection to %s" server))))
      (condition-case err
          (let ((text (mevedel-resource-mcp-extract-text
                       (mcp-read-resource connection uri))))
            (if (string-empty-p text)
                "(resource returned no text content)"
              text))
        (error
         (signal 'mevedel-resource-unavailable
                 (list (format "MCP resource read failed: %s"
                               (error-message-string err)))))))))

(defun mevedel-resource--agent-record (path session)
  "Return retained agent record for canonical PATH in SESSION."
  (when session
    (cdr (assoc path (mevedel-session-agent-registry session)))))

(defun mevedel-resource--agent-list-result (session &optional history-p)
  "Return a sorted retained-agent listing for SESSION.
When HISTORY-P is non-nil, list only identities with retained conversations."
  (let ((entries
         (cl-loop for item in (mevedel-agent-control-list-agents session)
                  for path = (plist-get item :path)
                  for record = (mevedel-resource--agent-record path session)
                  unless (equal path "/root")
                  when (or (not history-p)
                           (and record
                                (or (mevedel-agent-record-conversation-buffer
                                     record)
                                    (mevedel-agent-record-conversation-location
                                     record))))
                  collect
                  (let* ((address (format "%s://%s"
                                          (if history-p "history" "agent")
                                          (substring path 1)))
                         (activity (or (plist-get item :activity) "idle"))
                         (ready (and record
                                     (not (member activity
                                                  '("running" "starting"
                                                    "waiting"
                                                    "permission-blocked"
                                                    "interaction-blocked")))
                                     (mevedel-agent-control-settled-result
                                      record))))
                    (format "%s\t%s\t%s"
                            address
                            (or (plist-get item :role) "default")
                            (if ready "ready" "not-ready"))))))
    (if entries
        (string-join (sort entries #'string-lessp) "\n")
      (format "No retained %s agents" (if history-p "history" "agent")))))

(defconst mevedel-resource--json-null
  (make-symbol "mevedel-resource-json-null")
  "Sentinel for a parsed JSON null value.")

(defconst mevedel-resource--json-false
  (make-symbol "mevedel-resource-json-false")
  "Sentinel for a parsed JSON false value.")

(defconst mevedel-resource--json-missing
  (make-symbol "mevedel-resource-json-missing")
  "Sentinel for a missing JSON Pointer value.")

(defun mevedel-resource--json-parse (payload)
  "Parse complete JSON PAYLOAD with sentinels for null, false, and missing."
  (condition-case err
      (json-parse-string
       payload :object-type 'alist :array-type 'array
       :null-object mevedel-resource--json-null
       :false-object mevedel-resource--json-false)
    (error
     (signal 'mevedel-resource-unavailable
             (list (format "Agent result is not valid JSON: %s"
                           (error-message-string err)))))))

(defun mevedel-resource--json-pointer-value (value tokens)
  "Return VALUE selected by decoded RFC 6901 TOKENS, or signal missing."
  (let ((current value))
    (dolist (token tokens current)
      (setq current
            (cond
             ((and (listp current)
                   (or (null current)
                       (consp (car current))))
              (let ((entry (or (assoc token current)
                               (and (stringp token)
                                    (assoc (intern-soft token) current)))))
                (if entry (cdr entry) mevedel-resource--json-missing)))
             ((vectorp current)
              (if (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" token)
                  (let ((index (string-to-number token)))
                    (if (< index (length current))
                        (aref current index)
                      mevedel-resource--json-missing))
                mevedel-resource--json-missing))
             (t mevedel-resource--json-missing)))
      (when (eq current mevedel-resource--json-missing)
        (signal 'mevedel-resource-unavailable
                (list (format "JSON Pointer component is missing: %s" token)))))))

(defun mevedel-resource--json-render (value)
  "Render parsed JSON VALUE as readable scalar or deterministic JSON."
  (cond
   ((eq value mevedel-resource--json-null) "null")
   ((eq value mevedel-resource--json-false) "false")
   ((eq value t) "true")
   ((stringp value)
    value)
   ((numberp value) (number-to-string value))
   ((vectorp value)
    (concat "["
            (string-join
             (mapcar #'mevedel-resource--json-render (append value nil)) ",")
             "]"))
   ((listp value)
    (concat "{"
            (string-join
             (mapcar (lambda (entry)
                       (format "%s:%s"
                               (json-serialize (car entry))
                               (mevedel-resource--json-render (cdr entry))))
                     (sort (copy-sequence value)
                           (lambda (left right)
                             (string-lessp (car left) (car right)))))
             ",")
            "}"))
   (t (json-serialize (format "%s" value)))))

(defun mevedel-resource--agent-read (record parsed)
  "Return settled RECORD payload selected by PARSED address."
  (let ((settled (and record
                      (mevedel-agent-control-settled-result record))))
    (unless settled
      (signal 'mevedel-resource-unavailable
              (list "Retained agent has no settled result")))
    (let ((payload (plist-get settled :payload)))
      (if (not (plist-get parsed :fragment-p))
          payload
        (let* ((value (mevedel-resource--json-parse payload))
               (selected (mevedel-resource--json-pointer-value
                          value (plist-get parsed :pointer))))
          (mevedel-resource--json-render selected))))))

(defun mevedel-resource--history-hydrate (record session)
  "Hydrate cold RECORD from SESSION and return its conversation buffer.
The temporary parent buffer supplies the same session/workspace context used
by normal session resume; the hydrated retained buffer remains live after the
parent is discarded."
  (let ((root-buffer (generate-new-buffer
                      " *mevedel-resource-history-root*")))
    (unwind-protect
        (progn
          (with-current-buffer root-buffer
            (mevedel--transcript-org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace
                        (and session (mevedel-session-workspace session))))
          (mevedel-agent-persistence-restore-tree session root-buffer t)
          (let ((buffer (mevedel-agent-record-conversation-buffer record)))
            (if (buffer-live-p buffer)
                buffer
              (signal 'mevedel-resource-unavailable
                      (list "Retained agent conversation is unavailable")))))
      (when (buffer-live-p root-buffer)
        (kill-buffer root-buffer)))))

(defun mevedel-resource--history-read (record session)
  "Return concise Markdown projection for RECORD's retained conversation."
  (let ((buffer (and record
                     (mevedel-agent-record-conversation-buffer record))))
    (unless (buffer-live-p buffer)
      (setq buffer (mevedel-resource--history-hydrate record session)))
    (mevedel-agent-conversation-project-history buffer session)))

(defun mevedel-resource--skill-list-result (session context)
  "Return a canonical listing of discoverable skills."
  (let ((skills
         (cl-remove-if-not
          (lambda (skill)
            (or (not (fboundp 'mevedel-skills-skill-enabled-p))
                (mevedel-skills-skill-enabled-p skill)))
          (mevedel-resource--skill-list session context))))
    (if skills
        (string-join
         (mapcar (lambda (skill)
                   (format "%s\t%s"
                           (mevedel-resource--skill-address skill)
                           (or (mevedel-skill-description skill) "")))
                 (sort (copy-sequence skills)
                       (lambda (left right)
                         (string-lessp (mevedel-resource--skill-address left)
                                       (mevedel-resource--skill-address right)))))
         "\n")
      "No discoverable skills")))

(defun mevedel-resource--memory-search-roots (context session)
  "Return exact helper roots for the configured memory union.

Configured roots whose directory does not exist are excluded, matching
the union index read, which already tolerates missing roots."
  (mapcar
   (lambda (root)
     (list :path (plist-get root :dir)
           :address-prefix
           (concat "memory://" (mevedel-resource--memory-root-key root))
           :label (plist-get root :label)))
   (cl-remove-if-not
    (lambda (root) (file-directory-p (plist-get root :dir)))
    (mevedel-resource--memory-roots context session))))

(defun mevedel-resource--execute-logical (data options)
  "Execute virtual resource DATA using OPTIONS and return text."
  (ignore options)
  (let* ((scheme (plist-get data :scheme))
         (operation (plist-get data :operation))
         (parsed (plist-get data :parsed))
         (components (plist-get data :components))
         (session (plist-get data :session))
         (context (plist-get data :context))
         (root (plist-get data :root)))
    (pcase scheme
      ('local
       (if (null components)
           (if (eq operation 'read)
               (mevedel-resource--directory-list-result 'local root)
             (signal 'mevedel-resource-error
                     (list "Local search requires a file-backed directory")))
         (signal 'mevedel-resource-unavailable
                 (list "Local resource is file-backed"))))
      ('artifact
       (if (null components)
           (if (eq operation 'read)
               (mevedel-resource--directory-list-result 'artifact root)
             (signal 'mevedel-resource-error
                     (list "Artifact search requires a file-backed directory")))
         (signal 'mevedel-resource-unavailable
                 (list "Artifact resource is file-backed"))))
      ('skill
       (if (null (plist-get data :source-file))
           (mevedel-resource--skill-list-result session context)
         (signal 'mevedel-resource-unavailable
                 (list "Skill resource is file-backed"))))
      ('agent
       (if (null components)
           (mevedel-resource--agent-list-result session)
         (mevedel-resource--agent-read
          (plist-get data :record) parsed)))
      ('history
       (if (null components)
           (mevedel-resource--agent-list-result session t)
         (mevedel-resource--history-read
          (plist-get data :record) session)))
      ('memory
       (if (equal components '("root"))
           (pcase operation
             ('read
              (mevedel-system--memory-content
               (mevedel-resource--workspace context session)))
             ((or 'glob 'grep)
              (list :resource-search-roots
                    (mevedel-resource--memory-search-roots
                     context session))))
         (signal 'mevedel-resource-unavailable
                 (list "Memory topic is file-backed"))))
      ('mcp
       (cond
        ((null components)
         (mevedel-resource--mcp-list-result))
        ((null (cdr components))
         (let ((server-info (mevedel-resource--mcp-server (car components))))
           (unless server-info
             (signal 'mevedel-resource-unavailable
                     (list (format "Unknown MCP server: %s"
                                   (car components)))))
           (mevedel-resource--mcp-list-result server-info)))
        (t (mevedel-resource--mcp-read
            (car components) (cadr components)))))
      (_ (signal 'mevedel-resource-error
                 (list "Unknown resource provider"))))))

(defun mevedel-resource-within-root-p (path root)
  "Return non-nil when PATH resolves beneath ROOT, including ROOT itself.
This is an authorization answer, so on a remote target every symlink
and truename probe bypasses the TRAMP attribute cache: a stale cached
answer must not admit a path that has since been swapped."
  (when (and (stringp path) (stringp root))
    (let* ((remote-file-name-inhibit-cache t)
           (root (file-name-as-directory (expand-file-name root)))
           (path (expand-file-name path))
           (relative (file-relative-name path root))
           (cursor root)
           (symlink-p (file-symlink-p (directory-file-name root)))
           (lexical-p (not (or (equal relative "..")
                               (string-prefix-p
                                (file-name-as-directory "..") relative)))))
      (dolist (component (split-string relative "/" t))
        (setq cursor (expand-file-name component cursor))
        (when (file-symlink-p cursor)
          (setq symlink-p t)))
      (and lexical-p
           (not symlink-p)
           ;; `file-in-directory-p' needs the directory to exist.  A local
           ;; ApplyPatch may be the first operation that creates it, so keep
           ;; lexical containment as the authority while the target is
           ;; absent, and use the canonical check whenever both sides exist.
           (or (not (and (file-directory-p root) (file-exists-p path)))
               (let* ((true-root (file-name-as-directory
                                  (file-truename root)))
                      (true-path (file-truename path)))
                 (or (equal (directory-file-name true-root)
                            (directory-file-name true-path))
                     (file-in-directory-p true-path true-root))))))))

(defun mevedel-resource--skill-physical-path (skill root components)
  "Return the contained source or package path for SKILL.

COMPONENTS are relative to the selected skill package.  The source file is
checked through the same containment seam as package descendants rather
than trusting the discovery record's pathname."
  (if components
      (mevedel-resource--safe-path root components)
    (mevedel-resource--safe-path
     root
     (split-string (file-relative-name
                    (mevedel-skill-source-file skill) root)
                   "/" t))))

(defun mevedel-resource--refresh-data (data)
  "Re-resolve DATA's prepared locator against current session authority.

The authored address is not reparsed.  Session-relative roots, selected
skill sources, retained agent records, and memory roots are looked up again
before an authorized handler receives a backing path or virtual record."
  (let* ((scheme (plist-get data :scheme))
         (components (plist-get data :components))
         (session (plist-get data :session))
         (context (plist-get data :context))
         (operation (plist-get data :operation))
         root physical)
    ;; Availability belongs to the current authority, not to the first
    ;; discovery result captured by preparation.
    (setq data (plist-put data :unavailable-p nil))
    (cond
     ((memq scheme '(local artifact))
      (setq root (mevedel-resource--root scheme session)
            physical (mevedel-resource--safe-path root components))
      (when (and (eq scheme 'artifact)
                 (member ".mevedel-pending-executions" components))
        (setq data (plist-put data :unavailable-p t))))
     ((eq scheme 'skill)
      (unless (plist-get data :dynamic-p)
        (let ((skill (mevedel-resource--skill-for-digest
                      (plist-get data :source-key) session context)))
          (if (not skill)
              (setq data (plist-put data :unavailable-p t))
            (setq root (mevedel-resource--skill-root skill)
                  data (plist-put
                        data :source-file
                        (mevedel-skill-source-file skill))
                  physical
                  (if (memq operation '(glob grep))
                      (mevedel-resource--safe-path root components)
                    (mevedel-resource--skill-physical-path
                     skill root components)))))))
     ((memq scheme '(agent history))
      (when components
        (let ((record
               (and (equal (car components) "root")
                    (mevedel-resource--agent-record
                     (concat "/" (string-join components "/")) session))))
          (setq data (plist-put data :record record))
          (unless record
            (setq data (plist-put data :unavailable-p t))))))
     ((eq scheme 'memory)
      (unless (equal components '("root"))
        (let ((memory-root
               (mevedel-resource--memory-root-for-key
                (car components) context session)))
          (if (not memory-root)
              (setq data (plist-put data :unavailable-p t))
            (setq root (plist-get memory-root :dir)
                  physical (mevedel-resource--safe-path
                            root (cdr components))
                  data (plist-put data :memory-root memory-root)))))))
    (setq data (plist-put data :root root))
    (plist-put data :physical-path physical)))

(defun mevedel-resource-prepare (operation address context)
  "Prepare ADDRESS for OPERATION in CONTEXT without reading its content.

The returned value is opaque to callers.  Ordinary filesystem paths return
nil; malformed addresses and unsupported operation pairs signal validation
errors before any content or handler is reached."
  (when (and (stringp address)
             (mevedel-resource-address-like-p address))
    (condition-case err
        (let* ((parsed (mevedel-resource-parse-address address))
           (scheme (plist-get parsed :scheme))
           (components (plist-get parsed :components))
           (session (mevedel-resource--session context))
           (workspace (mevedel-resource--workspace context session))
           (data (list :resource-p t
                       :operation operation
                       :address address
                       :canonical (plist-get parsed :canonical)
                       :scheme scheme
                       :components components
                       :source-key (plist-get parsed :source-key)
                       :parsed parsed
                       :locator-class (plist-get parsed :locator-class)
                       :dynamic-p (plist-get parsed :dynamic-p)
                       :session session
                       :workspace workspace
                       :context context
                       :args (copy-tree (plist-get context :args))
                       :read-only-p (not (eq operation 'apply-patch))))
           physical root logical-p)
      (unless (or (eq operation 'read)
                  (and (memq operation '(glob grep))
                       (memq scheme '(local artifact skill memory)))
                  (and (eq operation 'apply-patch)
                       (eq scheme 'local)))
        (signal 'mevedel-resource-error
                (list (format "%s does not support %s addresses"
                              (upcase (symbol-name operation)) scheme))))
      (when (and (eq scheme 'skill)
                 (plist-get parsed :dynamic-p)
                 (not (eq operation 'read)))
        (signal 'mevedel-resource-error
                (list "Bare skill:// supports Read only")))
      ;; A bare directory address names a listing, never a patch endpoint.
      (when (and (null components)
                 (eq operation 'apply-patch))
        (signal 'mevedel-resource-error
                (list (format "Bare %s:// is not a patch target"
                              (symbol-name scheme)))))
      (cond
       ((memq scheme '(local artifact))
        (setq root (mevedel-resource--root scheme session)
              physical (and root
                            (mevedel-resource--safe-path root components))
              logical-p (and (null components)
                             (eq operation 'read)))
        (when (and (eq scheme 'artifact)
                   (member ".mevedel-pending-executions" components))
          (setq data (plist-put data :unavailable-p t))))
       ((eq scheme 'skill)
        (if (plist-get parsed :dynamic-p)
            (setq logical-p t)
          (let ((skill
                 (mevedel-resource--skill-for-digest
                  (plist-get parsed :source-key) session context)))
            (if (not skill)
                (setq data (plist-put data :unavailable-p t))
              (setq root (mevedel-resource--skill-root skill)
                    data (plist-put
                          data :source-file
                          (mevedel-skill-source-file skill))
                    data (plist-put
                          data :skill-address
                          (format "skill://%s@%s"
                                  (mevedel-resource-encode-component
                                   (plist-get parsed :name))
                                  (plist-get parsed :source-key))))
              (if (memq operation '(glob grep))
                  (setq physical (mevedel-resource--safe-path root components)
                        logical-p nil)
                (setq physical
                      (mevedel-resource--skill-physical-path
                       skill root components)))))))
       ((memq scheme '(agent history))
        (setq logical-p t)
        (if (null components)
            nil
          (let ((record
                 (and (equal (car components) "root")
                      (mevedel-resource--agent-record
                       (concat "/" (string-join components "/")) session))))
            (setq data (plist-put data :record record))
            (unless record
              (setq data (plist-put data :unavailable-p t))))))
       ((eq scheme 'memory)
        (if (equal components '("root"))
            (setq logical-p t)
          (let ((memory-root
                 (mevedel-resource--memory-root-for-key
                  (car components) context session)))
            (if (not memory-root)
                (setq data (plist-put data :unavailable-p t))
              (setq root (plist-get memory-root :dir)
                    physical (mevedel-resource--safe-path
                              root (cdr components))
                    data (plist-put data :memory-root memory-root))))))
       ((eq scheme 'mcp)
        (setq logical-p t)))
      (setq data (plist-put data :root root))
      (setq data (plist-put data :physical-path physical))
      (setq data (plist-put data :logical-p logical-p))
      (let ((attempt (make-symbol "mevedel-resource-attempt-")))
        (puthash attempt data mevedel-resource--attempt-table)
        (when-let ((cell (or (plist-get context :resource-attempts-cell)
                             mevedel-resource-attempts-cell)))
          (when (consp cell)
            (setcar cell (cons attempt (car cell)))))
        attempt))
      (mevedel-resource-error
       (signal 'mevedel-resource-error
               (list (format "Invalid resource address %s: %s"
                             address (error-message-string err))))))))

(defun mevedel-resource-attempt-address (attempt)
  "Return ATTEMPT's authored address."
  (plist-get (gethash attempt mevedel-resource--attempt-table) :address))

(defun mevedel-resource-execute (attempt &optional executor options)
  "Execute authorized opaque ATTEMPT.

For file-backed resources, EXECUTOR receives the private physical path and
authored address, preserving the initial filesystem-owner seam.  For virtual
resources, EXECUTOR receives a result descriptor and authored address; the
descriptor has `:virtual', `:result', `:address', and `:scheme'.  Without an
EXECUTOR, virtual resources return that descriptor.  OPTIONS replaces the
prepared operation options for virtual execution and is never reparsed as an
address.  No backing path is returned for a file-backed attempt without an
executor."
  (let ((data (gethash attempt mevedel-resource--attempt-table)))
    (unwind-protect
        (progn
          (unless (plist-get data :resource-p)
            (signal 'mevedel-resource-error
                    (list "Invalid resource execution attempt")))
          (setq data (mevedel-resource--refresh-data data))
          (when (plist-get data :unavailable-p)
            (signal 'mevedel-resource-unavailable
                    (list (format "Resource is unavailable: %s"
                                  (plist-get data :address)))))
          (if (plist-get data :logical-p)
              (let* ((result (mevedel-resource--execute-logical data options))
                     (descriptor (list :virtual t
                                       :result result
                                       :address (plist-get data :address)
                                       :scheme (plist-get data :scheme)
                                       :operation (plist-get data :operation))))
                (when (and (listp result)
                           (plist-member result :resource-search-roots))
                  (setq descriptor
                        (plist-put descriptor :resource-search-roots
                                   (plist-get result :resource-search-roots))))
                (if executor
                    (funcall executor descriptor (plist-get data :address))
                  descriptor))
            (unless (functionp executor)
              (signal 'mevedel-resource-error
                      (list "File-backed resources require an executor")))
            (funcall executor
                     (plist-get data :physical-path)
                     (plist-get data :address))))
      (remhash attempt mevedel-resource--attempt-table))))

(defun mevedel-resource-discard-attempts (attempts)
  "Discard opaque resource ATTEMPTS that will not be executed."
  (dolist (attempt attempts)
    (remhash attempt mevedel-resource--attempt-table))
  nil)

(defun mevedel-resource-current-attempt (address)
  "Return the dynamically active attempt for authored ADDRESS."
  (cdr (assoc address mevedel-resource-current-attempts)))

(defun mevedel-resource-artifact-address (path session)
  "Return the logical artifact address for PATH owned by SESSION."
  (when-let* ((root (mevedel-resource--root 'artifact session))
              (path (expand-file-name path))
              ((mevedel-resource-within-root-p path root))
              (relative (mevedel-resource--canonical-relative path root))
              ((not (string-match-p
                     "\\`\\.mevedel-pending-executions\\(?:/\\|\\'\\)"
                     relative))))
    (concat "artifact://"
            (mapconcat #'mevedel-resource-encode-component
                       (split-string relative "/" t)
                       "/"))))

(provide 'mevedel-resource)
;;; mevedel-resource.el ends here
