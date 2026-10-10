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
(declare-function cl-count-if "cl-lib" (predicate sequence &rest args))
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
(declare-function mevedel-agent-persistence-ensure-conversation
                  "mevedel-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'mevedel-agent-persistence-ensure-conversation
  "mevedel-agent-persistence")

;; `mevedel-execution'
(declare-function mevedel-execution-list-user "mevedel-execution" (session))

;; `mevedel-journal-index'
(declare-function mevedel-journal-index-entries "mevedel-journal-index" (workspace &optional cached-only))
(autoload 'mevedel-journal-index-entries "mevedel-journal-index")

;; `mevedel-journal-store'
(declare-function mevedel-journal-store-directory "mevedel-journal-store" (root))
(declare-function mevedel-journal-store-entries "mevedel-journal-store" (root))
(declare-function mevedel-journal-store-file-name-p "mevedel-journal-store" (name))
(declare-function mevedel-journal-store-read "mevedel-journal-store" (root file))
(declare-function mevedel-journal-store-recall-p "mevedel-journal-store" (entry &optional now))
(autoload 'mevedel-journal-store-directory "mevedel-journal-store")
(autoload 'mevedel-journal-store-entries "mevedel-journal-store")
(autoload 'mevedel-journal-store-file-name-p "mevedel-journal-store")
(autoload 'mevedel-journal-store-read "mevedel-journal-store")
(autoload 'mevedel-journal-store-recall-p "mevedel-journal-store")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-segment-number
                  "mevedel-session-artifacts" (logical))
(autoload 'mevedel-session-artifacts-segment-number "mevedel-session-artifacts")

;; `mevedel-skills-core'
(declare-function mevedel-skill-description "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-name "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-plugin-name "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-raw-name "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source-dir "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source-family "mevedel-skills-core" (skill) t)
(declare-function mevedel-skill-source-file "mevedel-skills-core" (skill) t)
(declare-function mevedel-skills-skill-enabled-p "mevedel-skills-core" (skill))
(declare-function mevedel-skills-source-key "mevedel-skills-core" (source-file))
(declare-function mevedel-skills-scan "mevedel-skills-core"
                  (&optional workspace-root dirs workspace))
(autoload 'mevedel-skills-scan "mevedel-skills-core")

;; `mevedel-shared-editing'
(declare-function mevedel-shared-editing-list "mevedel-shared-editing" (workspace))
(declare-function mevedel-shared-editing-present-p "mevedel-shared-editing" (workspace id))
(autoload 'mevedel-shared-editing-list "mevedel-shared-editing")
(autoload 'mevedel-shared-editing-present-p "mevedel-shared-editing")

;; `mevedel-structs'
(declare-function mevedel-agent-path-p "mevedel-structs" (path))
(declare-function mevedel-session-agent-registry "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-attached-artifacts "mevedel-structs" (cl-x) t)
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
(declare-function mevedel-library-source-directory "mevedel-utilities" (file))
(autoload 'mevedel--transcript-org-mode "mevedel-utilities")
(autoload 'mevedel-library-source-directory "mevedel-utilities")

(defconst mevedel-resource-supported-schemes
  '(work artifact skill agent history memory mcp shared mevedel)
  "Closed set of resource address schemes understood by mevedel.")

(defconst mevedel-resource--unreserved
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
  "RFC 3986 unreserved bytes.")

(defconst mevedel-resource--skill-alias-sources
  '(("local-mevedel" . local-mevedel)
    ("local-agents" . local-agents)
    ("global-mevedel" . global-mevedel)
    ("global-agents" . global-agents)
    ("bundled" . bundled)
    ("managed" . managed)
    ("plugin" . plugin))
  "Closed readable source names accepted by `skill://' aliases.")

(defconst mevedel-resource--memory-alias-keys
  '("local-mevedel" "local-agents" "global-mevedel" "global-agents")
  "Closed readable root keys accepted by `memory://' aliases.")

(defvar mevedel-resource--source-dir
  (mevedel-library-source-directory
   (or load-file-name buffer-file-name (locate-library "mevedel-resource")))
  "Canonical package directory containing Mevedel's documentation.")

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

(defun mevedel-resource-error-message (failure &optional address private-paths)
  "Format FAILURE once, naming ADDRESS and hiding PRIVATE-PATHS.
Resource conditions already contain a useful message; omit their redundant
condition labels.  Other conditions retain their ordinary diagnostic text."
  (let ((message (if (memq (car failure)
                           '(mevedel-resource-error mevedel-resource-unavailable))
                     (cadr failure)
                   (error-message-string failure))))
    (when address
      (dolist (path (sort (delete-dups (delq nil (copy-sequence private-paths)))
                          (lambda (left right) (> (length left) (length right)))))
        (dolist (variant (delete-dups
                          (list (directory-file-name path)
                                (file-local-name (directory-file-name path)))))
          (setq message (string-replace
                         (concat variant "/")
                         (if (string-suffix-p "/" address) address
                           (concat address "/"))
                         message))
          (setq message (string-replace variant address message))))
      (unless (string-search address message)
        (setq message (format "%s (%s)" message address))))
    message))

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
        (if (string-search (char-to-string byte) mevedel-resource--unreserved)
            (push (char-to-string byte) result)
          (push (format "%%%02X" byte) result))))
    (apply #'concat (nreverse result))))

(defun mevedel-resource--decode-component (raw &optional allow-separator)
  "Decode one RAW component and reject malformed escapes or unsafe names.

Characters outside the unreserved set may appear literally or escaped;
the caller derives the canonical spelling from the decoded value.

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
          (dolist (byte (append (encode-coding-string (string char) 'utf-8 t)
                                nil))
            (push byte bytes))
          (setq index (1+ index)))))
    (let ((decoded
           (decode-coding-string
            (apply #'unibyte-string (nreverse bytes)) 'utf-8 t)))
      ;; Emacs preserves malformed UTF-8 as raw-byte characters and can
      ;; decode code points above Unicode's upper bound.
      (when (string-match-p "[^\u0000-\U0010ffff]" decoded)
        (signal 'mevedel-resource-error '("Invalid UTF-8 in resource address")))
      (when (or (string-empty-p decoded)
                (mevedel-resource--has-control-character-p decoded)
                (and (not allow-separator)
                     (member decoded '("." "..")))
                (and (not allow-separator) (string-match-p "/" decoded)))
        (signal 'mevedel-resource-error
                (list "Unsafe resource path component")))
      decoded)))

(defun mevedel-resource--parse-components (tail)
  "Parse slash-separated path TAIL into decoded canonical components."
  (if (string-empty-p tail)
      nil
    (let ((raw-components (split-string tail "/" nil)))
      (when (member "" raw-components)
        (signal 'mevedel-resource-error
                (list "Empty resource path component")))
      (mapcar #'mevedel-resource--decode-component raw-components))))

(defun mevedel-resource--canonical-components (components)
  "Return canonical slash-separated encoding for COMPONENTS."
  (mapconcat #'mevedel-resource-encode-component components "/"))

(defun mevedel-resource--decode-json-pointer (fragment)
  "Validate decoded JSON Pointer FRAGMENT and return its tokens.

FRAGMENT is the decoded URI fragment, not the raw address spelling.  The
empty fragment selects the complete JSON value."
  (when (string-match-p "[^\u0000-\U0010ffff]" fragment)
    (signal 'mevedel-resource-error '("Invalid UTF-8 in JSON Pointer")))
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
               (if (or (= character ?/)
                       (string-search (char-to-string character)
                                      mevedel-resource--unreserved))
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
           (alias-source
            (and (not (string-search "@" head))
                 (cdr (assoc (mevedel-resource--decode-component head)
                             mevedel-resource--skill-alias-sources)))))
      (if alias-source
          (let* ((parts (mevedel-resource--parse-components tail))
                 (plugin-p (eq alias-source 'plugin))
                 (minimum (if plugin-p 3 2)))
            (when (< (length parts) minimum)
              (signal 'mevedel-resource-error
                      (list "Skill alias is missing its skill name")))
            (list :components (nthcdr minimum parts)
                  :alias-source alias-source
                  :plugin-name (and plugin-p (cadr parts))
                  :raw-name (nth (1- minimum) parts)
                  :canonical-tail
                  (mevedel-resource--canonical-components parts)
                  :locator-class 'alias
                  :dynamic-p nil))
        (let* ((path (and slash (substring tail (1+ slash))))
               (at (string-match "@" head)))
          (when (or (null at)
                    (= at 0)
                    (= (1+ at) (length head))
                    (string-match "@" (substring head (1+ at)))
                    (not (mevedel-resource--lowercase-digest-p
                          (substring head (1+ at)))))
            (signal
             'mevedel-resource-error
             (list "Skill address requires a name and lowercase source digest")))
          (let* ((name (mevedel-resource--decode-component
                        (substring head 0 at)))
                 (source-key (substring head (1+ at)))
                 (components (if slash
                                 (mevedel-resource--parse-components path)
                               nil)))
            (list :components components :name name :source-key source-key
                  :dynamic-p nil)))))))

(defun mevedel-resource--parse-memory-tail (tail)
  "Parse the memory-specific TAIL and return its locator fields."
  (let ((components (mevedel-resource--parse-components tail)))
    (cond
     ((equal (car components) "journal")
      (unless (or (null (cdr components))
                  (and (= (length components) 2)
                       (mevedel-journal-store-file-name-p (cadr components))))
        (signal 'mevedel-resource-error
                '("Journal addresses use memory://journal/ or name one public entry")))
      (list :components components :dynamic-p (null (cdr components))
            :locator-class (unless (null (cdr components)) 'workspace-relative)))
     ((equal components '("root"))
      (list :components components :dynamic-p t))
     ((and (>= (length components) 2)
           (or (mevedel-resource--lowercase-digest-p (car components))
               (member (car components)
                       mevedel-resource--memory-alias-keys)))
      (list :components components :dynamic-p nil))
     (t
      (signal 'mevedel-resource-error
              (list "Memory address requires 'root', 'journal/', or a root key and path"))))))

(defun mevedel-resource--parse-agent-history-components (components)
  "Validate retained-agent COMPONENTS for agent or history resources."
  (if (null components)
      (list :components nil :dynamic-p t)
    (unless (and (> (length components) 1)
                 (equal (car components) "root"))
      (signal 'mevedel-resource-error
              (list "Agent and history addresses require a retained /root path")))
    (unless (mevedel-agent-path-p
             (concat "/" (string-join components "/")))
      (signal 'mevedel-resource-error
              (list "Agent and history addresses require a canonical agent path")))
    (list :components components :dynamic-p nil)))

(defun mevedel-resource--parse-mcp-tail (tail)
  "Parse the MCP-specific TAIL and return its locator fields."
  (if (string-empty-p tail)
      (list :components nil :dynamic-p t)
    (let ((raw-components (split-string tail "/" nil)))
      (when (or (member "" raw-components) (> (length raw-components) 2))
        (signal 'mevedel-resource-error
                (list "MCP address has an invalid component count")))
      (list :components
            (mapcar (lambda (raw) (mevedel-resource--decode-component raw t))
                    raw-components)
            :dynamic-p (= (length raw-components) 1)))))

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

(defun mevedel-resource--memory-root-address-key (root roots)
  "Return ROOT's readable alias key, or its digest.

The alias is used only when it names exactly one root among ROOTS; the
digest remains the authority a readable key resolves to."
  (let ((alias (plist-get root :alias)))
    (if (and alias
             (member alias mevedel-resource--memory-alias-keys)
             (= 1 (cl-count-if
                   (lambda (other) (equal alias (plist-get other :alias)))
                   roots)))
        alias
      (mevedel-resource--memory-root-key root))))

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

(defun mevedel-resource--skill-alias-source (skill)
  "Return SKILL's readable alias source, or nil when it has none."
  (pcase (mevedel-skill-source skill)
    ('project
     (pcase (mevedel-skill-source-family skill)
       ('mevedel 'local-mevedel)
       ('agents 'local-agents)))
    ('user
     (pcase (mevedel-skill-source-family skill)
       ('mevedel 'global-mevedel)
       ('agents 'global-agents)))
    ((or 'bundled 'managed 'plugin)
     (mevedel-skill-source skill))))

(defun mevedel-resource--skill-alias-address (skill)
  "Return SKILL's readable alias address, or nil when incomplete."
  (when-let* ((source (mevedel-resource--skill-alias-source skill))
              (raw-name (mevedel-skill-raw-name skill))
              ((or (not (eq source 'plugin))
                   (mevedel-skill-plugin-name skill))))
    (concat "skill://" (symbol-name source) "/"
            (if (eq source 'plugin)
                (concat
                 (mevedel-resource-encode-component
                  (mevedel-skill-plugin-name skill)) "/")
              "")
            (mevedel-resource-encode-component raw-name))))

(defun mevedel-resource--skill-for-alias (parsed session context)
  "Resolve PARSED readable skill alias uniquely in SESSION and CONTEXT."
  (let (matches)
    (dolist (skill (mevedel-resource--skill-list session context))
      (when (and
             (eq (plist-get parsed :alias-source)
                 (mevedel-resource--skill-alias-source skill))
             (equal (plist-get parsed :raw-name)
                    (mevedel-skill-raw-name skill))
             (equal (plist-get parsed :plugin-name)
                    (mevedel-skill-plugin-name skill))
             (or (not (fboundp 'mevedel-skills-skill-enabled-p))
                 (mevedel-skills-skill-enabled-p skill)))
        (push skill matches)))
    (cond
     ((null matches)
      (signal 'mevedel-resource-error
              (list "Skill alias does not name an enabled skill; Read skill:// to discover available skills")))
     ((cdr matches)
      (signal 'mevedel-resource-error
              (list "Skill alias is ambiguous; Read skill:// and use an exact source address")))
     (t (car matches)))))

(defun mevedel-resource--skill-root (skill)
  "Return SKILL's package root, deriving it from its source when needed."
  (or (mevedel-skill-source-dir skill)
      (when-let* ((source (mevedel-skill-source-file skill)))
        (file-name-directory source))))

(defun mevedel-resource--skill-address (skill &optional components)
  "Return the canonical exact address for SKILL and COMPONENTS."
  (concat
   (format "skill://%s@%s"
           (mevedel-resource-encode-component
            (mevedel-skill-name skill))
           (mevedel-resource-skill-digest
            (mevedel-skill-source-file skill)))
   (when components
     (concat "/" (mevedel-resource--canonical-components components)))))

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
         (shared-items (and (memq scheme '(nil shared))
                            session
                            (mevedel-shared-editing-list
                             (mevedel-session-workspace session))))
         (servers (and (memq scheme '(nil mcp))
                       (fboundp 'mcp-hub-get-servers)
                       (condition-case nil
                           (mcp-hub-get-servers)
                         (error nil)))))
    (list :roots
          (delq nil
                (list
                 (and (memq scheme '(nil work))
                      (cons 'work
                            (mevedel-resource--root 'work session)))
                 (and (memq scheme '(nil artifact))
                      (cons 'artifact
                            (mevedel-resource--root 'artifact session)))
                 (and (memq scheme '(nil mevedel))
                      (cons 'mevedel
                            (mevedel-resource--root 'mevedel nil)))))
          :shared-root
          (when (memq scheme '(nil work))
            (mevedel-resource-work-shared-directory
             (mevedel-resource--workspace context session)))
          :decode-component #'mevedel-resource--decode-component
          :saved-history-p
          (and (memq scheme '(nil history))
               (mevedel-resource--workspace context session) t)
          :safe-path #'mevedel-resource--safe-path
          :skills
          (delq nil
                (mapcar
                 (lambda (skill)
                   (when-let* ((address
                               (condition-case nil
                                   (mevedel-resource--skill-address skill)
                                 (error nil))))
                     (list :skill skill
                           :address address
                           :alias
                           (mevedel-resource--skill-alias-address skill))))
                 skills))
          :agents
          (mapcar
           (lambda (item)
             (let* ((path (plist-get item :path))
                    (record (mevedel-resource--agent-record path session)))
               (list :item item :record record
                     :history-p
                     (if (equal path "/root")
                         (buffer-live-p (mevedel-session-root-buffer session))
                       (and record
                            (or (buffer-live-p
                                 (mevedel-agent-record-conversation-buffer record))
                                (mevedel-agent-record-conversation-location
                                 record)))))))
           agents)
          :memory-roots
          (delq nil
                (mapcar (lambda (root)
                          (when (or (null scheme)
                                    (not (file-remote-p
                                          (plist-get root :dir))))
                            (list
                             :root root
                             :key (mevedel-resource--memory-root-address-key
                                   root memory-roots))))
                        memory-roots))
          :journal
          (when (memq scheme '(nil memory))
            (mapcar (lambda (entry) (list :file (plist-get entry :file)))
                    (mevedel-journal-index-entries
                     (mevedel-resource--workspace context session) scheme)))
          :shared-items shared-items
          :mcp-servers servers)))

(defun mevedel-resource--memory-root-for-key (key context session)
  "Return the configured memory root whose digest or alias is KEY."
  (let ((roots (mevedel-resource--memory-roots context session)))
    (if (mevedel-resource--lowercase-digest-p key)
        (cl-find-if
         (lambda (root)
           (equal key (mevedel-resource--memory-root-key root)))
         roots)
      (let ((matches
             (cl-remove-if-not
              (lambda (root) (equal key (plist-get root :alias)))
              roots)))
        (when (cdr matches)
          (signal 'mevedel-resource-error
                  (list "Memory root alias is ambiguous")))
        (car matches)))))

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
yield makes them public under `executions/'; the installed documentation
listing includes only Markdown files."
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
                    (unless (or (and (eq scheme 'artifact)
                                     (string-match-p
                                      "\\`\\.mevedel-pending-executions\\(?:/\\|\\'\\)"
                                      relative))
                                (and (eq scheme 'mevedel)
                                     (not (string-suffix-p ".md" relative))))
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
  "Parse ADDRESS and return a locator plist.

ADDRESS may spell components with literal characters or escapes and may end
a directory with one slash; `:canonical' is the single spelling of its
identity.  The plist contains decoded `:components', canonical `:canonical', and
`:locator-class' (`exact', `alias', `session-relative', `workspace-relative',
or `dynamic').
Physical resolution is intentionally not performed here."
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
        (setq fragment-data
              (mevedel-resource--decode-fragment (substring tail (1+ fragment-p))))
        (setq tail (substring tail 0 fragment-p)))
      ;; Directories are commonly written with a trailing slash; it names
      ;; nothing beyond the directory itself.
      (when (and (> (length tail) 1) (string-suffix-p "/" tail))
        (setq tail (substring tail 0 -1)))
      (when (and (not (eq scheme 'agent)) (string-match "#" tail))
        (signal 'mevedel-resource-error
                (list "Fragments are not supported by this resource scheme")))
      (let* ((specific
              (pcase scheme
                ('skill (mevedel-resource--parse-skill-tail tail))
                ('history
                 (let ((parts (mevedel-resource--parse-components tail)))
                   (cond
                    ((equal parts '("root"))
                     (list :components parts :dynamic-p nil))
                    ((equal (car parts) "saved")
                     (unless (and (<= (length parts) 3)
                                  (or (< (length parts) 3)
                                      (mevedel-session-artifacts-segment-number (caddr parts))))
                       (signal 'mevedel-resource-error '("Saved history names a session and canonical segment")))
                     (list :components parts :dynamic-p (= (length parts) 1)
                           :locator-class (unless (= (length parts) 1)
                                            'workspace-relative)))
                    (t (mevedel-resource--parse-agent-history-components parts)))))
                ('agent
                 (mevedel-resource--parse-agent-history-components
                  (mevedel-resource--parse-components tail)))
                ('memory (mevedel-resource--parse-memory-tail tail))
                ('mcp (mevedel-resource--parse-mcp-tail tail))
                (_ (list :components (mevedel-resource--parse-components tail)
                         :dynamic-p (string-empty-p tail)))))
             (components (plist-get specific :components))
             (fragment (plist-get fragment-data :fragment))
             (canonical-tail
              (cond
               ((eq scheme 'skill)
                (cond
                 ((plist-get specific :canonical-tail)
                  (plist-get specific :canonical-tail))
                 ((plist-get specific :name)
                    (concat (mevedel-resource-encode-component
                             (plist-get specific :name)) "@"
                            (plist-get specific :source-key)
                            (if components
                                (concat "/"
                                        (mevedel-resource--canonical-components
                                         components))
                              "")))
                 (t "")))
               ((and (eq scheme 'memory) (equal components '("journal")))
                "journal/")
               (t (mevedel-resource--canonical-components components))))
             (canonical (concat prefix canonical-tail
                                (if fragment-p
                                    (concat "#" (plist-get fragment-data :raw))
                                  "")))
             (class
              (or (plist-get specific :locator-class)
                  (cond
                   ((plist-get specific :dynamic-p) 'dynamic)
                   ((and (eq scheme 'work) (mevedel-resource--shared-work-p components))
                    'exact)
                   ((memq scheme '(work artifact agent history shared))
                    'session-relative)
                   (t 'exact)))))
        (when (and (eq scheme 'agent)
                   fragment-p
                   (null components))
          (signal 'mevedel-resource-error
                  (list "Agent JSON Pointer requires a canonical agent path")))
        (list :scheme scheme
              :components components
              :name (plist-get specific :name)
              :source-key (plist-get specific :source-key)
              :alias-source (plist-get specific :alias-source)
              :plugin-name (plist-get specific :plugin-name)
              :raw-name (plist-get specific :raw-name)
              :fragment fragment
              :fragment-p fragment-p
              :pointer (and fragment-data
                            (plist-get fragment-data :tokens))
              :canonical canonical
              :locator-class class
              :dynamic-p (eq class 'dynamic))))))

(defun mevedel-resource--root (scheme session)
  "Return the physical root for SCHEME and SESSION, without creating it."
  (if (eq scheme 'mevedel)
      (file-name-concat mevedel-resource--source-dir "docs")
    (let ((save-path (and session (mevedel-session-save-path session))))
      (and save-path
           (pcase scheme
             ('work (file-name-concat save-path "local"))
             ('artifact (file-name-concat save-path "tool-results")))))))

(defconst mevedel-resource--shared-work-component "shared"
  "First `work://' component that addresses the workspace's shared files.")

(defconst mevedel-resource-shared-work-address
  (concat "work://" mevedel-resource--shared-work-component)
  "Canonical address of the workspace's shared working files.")

(defun mevedel-resource-work-shared-directory (workspace)
  "Return WORKSPACE's shared working directory without creating or probing it.
Resolution for access goes through `mevedel-resource--work-location', which
also rejects symlink escapes."
  (when workspace
    (file-name-concat (mevedel-workspace-root workspace)
                      ".mevedel" mevedel-resource--shared-work-component)))

(defun mevedel-resource--shared-work-p (components)
  "Return non-nil when work COMPONENTS address the workspace's shared files."
  (equal (car components) mevedel-resource--shared-work-component))

(defun mevedel-resource-session-work-p (address)
  "Return non-nil if ADDRESS names a session-owned work descendant."
  (when (and (stringp address) (string-prefix-p "work://" address))
    (condition-case nil
        (let ((parts (plist-get (mevedel-resource-parse-address address) :components)))
          (and parts (not (mevedel-resource--shared-work-p parts))))
      (mevedel-resource-error nil))))

(defun mevedel-resource--work-location (components context session)
  "Return (ROOT . RELATIVE) for work COMPONENTS in CONTEXT and SESSION.
Shared components resolve below the workspace's contained shared directory,
other components below the session's own work root.  ROOT is nil when the
owner is unavailable."
  (if (mevedel-resource--shared-work-p components)
      (cons (when-let* ((workspace (mevedel-resource--workspace context session)))
              (mevedel-resource--safe-path (mevedel-workspace-root workspace)
                                          (list ".mevedel" mevedel-resource--shared-work-component)))
            (cdr components))
    (cons (mevedel-resource--root 'work session) components)))

(defun mevedel-resource--mevedel-path-available-p
    (root path components operation)
  "Return non-nil when PATH is an exposed Mevedel document scope.

ROOT must be the installed documentation directory.  Bare addresses expose
the corpus, Markdown files are exact resources, and directories are valid only
as Glob or Grep scopes."
  (and root
       (file-directory-p root)
       (not (file-symlink-p (directory-file-name root)))
       (or (null components)
           (and path
                (cond
                 ((file-directory-p path)
                  (memq operation '(glob grep)))
                 ((file-regular-p path)
                  (string-suffix-p ".md" path)))))))

(defconst mevedel-resource--shared-identity "\\`[a-zA-Z0-9_-]\\{1,80\\}\\'"
  "Identity of a shared item, element, block or image.")

(defun mevedel-resource--shared-shape-p (components)
  "Return non-nil when COMPONENTS name a `shared://' resource.
Items are `ID' with `view.png', `comments', `history', `elements/ID' and
`images/KEY' descendants; `library' lists element libraries, optionally one
library by name, each with a `sheet.png'."
  (cl-flet ((id-p (value)
              (and (stringp value)
                   (string-match-p mevedel-resource--shared-identity value))))
    (pcase components
      ('nil t)
      (`("library") t)
      (`("library" ,_) t)
      (`("library" ,_ "sheet.png") t)
      (`(,id) (id-p id))
      (`(,id ,(or "view.png" "comments" "history")) (id-p id))
      (`(,id ,(or "elements" "images") ,name) (and (id-p id) (id-p name))))))

(defun mevedel-resource--shared-available-p (components session)
  "Return non-nil when shared COMPONENTS resolve in SESSION's workspace.
Item addresses need an existing item; listings and libraries always resolve."
  (or (null components)
      (equal (car components) "library")
      (and session
           (mevedel-shared-editing-present-p (mevedel-session-workspace session)
                                             (car components))
           t)))

(defun mevedel-resource--shared-list-result (session)
  "Return the workspace's shared item listing for a bare `shared://' Read.
Items attached to SESSION are marked."
  (let ((items (and session (mevedel-shared-editing-list
                             (mevedel-session-workspace session))))
        (attached (and session (mevedel-session-attached-artifacts session))))
    (concat
     (if items
         (mapconcat (lambda (item)
                      (format "shared://%s\t%s %S%s"
                              (plist-get item :id) (plist-get item :kind)
                              (plist-get item :title)
                              (if (member (plist-get item :id) attached)
                                  " · attached" "")))
                    items "\n")
       "No shared whiteboards or documents in this project; SharedCreate starts one.")
     "\nshared://library\tWhiteboard element libraries for SharedEdit insert")))

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
          (let* ((response (mcp-read-resource connection uri))
                 (text (mevedel-resource-mcp-extract-text response)))
            (if (string-empty-p text)
                (format "%s (%s)"
                        (if (seq-empty-p (plist-get response :contents))
                            "MCP resource returned no content"
                          "MCP resource returned no text; this Read surface exposes text content only")
                        (mevedel-resource--mcp-address server uri))
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
  "Return a sorted agent resource listing for SESSION.
When HISTORY-P is non-nil, include root and retained conversation histories."
  (let ((entries
         (cl-loop for entry in
                  (plist-get
                   (mevedel-resource-completion-metadata
                    (list :session session) (if history-p 'history 'agent))
                   :agents)
                  for item = (plist-get entry :item)
                  for path = (plist-get item :path)
                  for record = (plist-get entry :record)
                  when (if history-p
                           (plist-get entry :history-p)
                         (not (equal path "/root")))
                  collect
                  (let* ((address (format "%s://%s"
                                          (if history-p "history" "agent")
                                          (substring path 1)))
                         (activity (or (plist-get item :activity) "idle"))
                         (ready (or (and history-p (equal path "/root"))
                                    (and record
                                         (not (member activity
                                                      '("running" "starting"
                                                        "waiting"
                                                        "permission-blocked"
                                                        "interaction-blocked")))
                                         (mevedel-agent-control-settled-result
                                          record)))))
                    (format "%s\t%s\t%s"
                            address
                            (or (plist-get item :role) "default")
                            (if ready "ready" "not-ready"))))))
    (if entries
        (string-join (sort entries #'string-lessp) "\n")
      (if history-p "No conversation histories"
        "No retained agents"))))

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
             (list (format "Agent result is not valid JSON; Read without a JSON pointer to inspect the full result: %s"
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

(defun mevedel-resource--json-render (value &optional nested)
  "Render parsed JSON VALUE as readable scalar or deterministic JSON.
When NESTED in an array or object, retain JSON string quoting."
  (cond
   ((eq value mevedel-resource--json-null) "null")
   ((eq value mevedel-resource--json-false) "false")
   ((eq value t) "true")
   ((stringp value)
    (if nested (json-serialize value) value))
   ((numberp value) (number-to-string value))
   ((vectorp value)
    (concat "["
            (string-join
             (mapcar (lambda (item) (mevedel-resource--json-render item t))
                     (append value nil)) ",")
             "]"))
   ((listp value)
    (concat "{"
            (string-join
             (mapcar (lambda (entry)
                       (format "%s:%s"
                               (json-serialize (symbol-name (car entry)))
                               (mevedel-resource--json-render (cdr entry) t)))
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
              (list "Retained agent has no settled result yet; wait for completion or inspect its history:// conversation")))
    (let ((payload (plist-get settled :payload)))
      (if (not (plist-get parsed :fragment-p))
          payload
        (let* ((value (mevedel-resource--json-parse payload))
               (selected (mevedel-resource--json-pointer-value
                          value (plist-get parsed :pointer))))
          (mevedel-resource--json-render selected))))))

(defun mevedel-resource--history-hydrate (record session)
  "Load only RECORD's conversation from SESSION for history inspection.
Use the live root's context when available; otherwise use a temporary read-only
context.  Other retained agents and their activity remain untouched."
  (let* ((live (mevedel-session-root-buffer session))
         (temporary (not (buffer-live-p live)))
         (root-buffer (if temporary
                          (generate-new-buffer " *mevedel-resource-history-root*")
                        live)))
    (unwind-protect
        (progn
          (when temporary
            (with-current-buffer root-buffer
              (mevedel--transcript-org-mode)
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace (mevedel-session-workspace session))))
          (mevedel-agent-persistence-ensure-conversation
           session record root-buffer
           (or temporary (buffer-local-value 'mevedel-session--read-only-mode root-buffer))))
      (when (and temporary (buffer-live-p root-buffer))
        (kill-buffer root-buffer)))))

(defun mevedel-resource--history-read (record session)
  "Return concise Markdown for RECORD, or SESSION's root when RECORD is nil."
  (let ((buffer (if record
                    (mevedel-agent-record-conversation-buffer record)
                  (and session (mevedel-session-root-buffer session)))))
    (when (and record (not (buffer-live-p buffer)))
      (setq buffer (mevedel-resource--history-hydrate record session)))
    (unless (buffer-live-p buffer)
      (signal 'mevedel-resource-unavailable
              (list "Root conversation is unavailable")))
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
        (let ((alias-counts (make-hash-table :test #'equal))
              rows)
          (dolist (skill skills)
            (when-let* ((alias (mevedel-resource--skill-alias-address skill)))
              (puthash alias (1+ (gethash alias alias-counts 0)) alias-counts)))
          (dolist (skill skills)
            (let ((description (or (mevedel-skill-description skill) ""))
                  (alias (mevedel-resource--skill-alias-address skill)))
              (push (cons (mevedel-resource--skill-address skill) description)
                    rows)
              (when (and alias (= 1 (gethash alias alias-counts)))
                (push (cons alias description) rows))))
          (string-join
           (mapcar (lambda (row) (format "%s\t%s" (car row) (cdr row)))
                   (sort rows (lambda (left right)
                                (string-lessp (car left) (car right)))))
           "\n"))
      "No discoverable skills")))

(defun mevedel-resource--memory-search-roots (context session)
  "Return exact helper roots for the configured memory union.

Configured roots whose directory does not exist are excluded, matching
the union index read, which already tolerates missing roots."
  (let ((roots (mevedel-resource--memory-roots context session)))
    (mapcar
     (lambda (root)
       (list :path (plist-get root :dir)
             :address-prefix
             (concat "memory://"
                     (mevedel-resource--memory-root-address-key root roots))
             :label (plist-get root :label)))
     (cl-remove-if-not
      (lambda (root) (file-directory-p (plist-get root :dir)))
      roots))))

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
      ('work
       (let* ((shared (car (mevedel-resource--work-location
                           (list mevedel-resource--shared-work-component) context session)))
              (shared-p (equal components '("shared")))
              (empty (if shared-p
                         "No shared working files yet. Create a file under work://shared/ using ApplyPatch when workspace writes are permitted."
                       "No working files found under work://."))
              (roots (cl-remove-if-not
                      (lambda (entry)
                        (and (plist-get entry :path)
                             (file-directory-p (plist-get entry :path))))
                      (list (list :path (unless shared-p root) :address "work://")
                            (list :path shared :address-prefix mevedel-resource-shared-work-address)))))
         (if (eq operation 'read)
             (let ((files
                    (cl-loop for entry in roots
                             append
                             (mapcar
                              (lambda (file)
                                (mevedel-resource--logical-address
                                 'work (append (when (plist-get entry :address-prefix)
                                                 '("shared"))
                                               (split-string file "/" t))))
                              (mevedel-resource--file-list (plist-get entry :path))))))
               (if files (string-join files "\n") empty))
           (list :resource-search-roots roots :empty-result empty))))
      ('artifact
       (if (null components)
           (if (eq operation 'read)
               (mevedel-resource--directory-list-result 'artifact root)
             (list :resource-search-roots
                   (when (and root (file-directory-p root))
                     (list (list :path root :address "artifact://")))
                   :empty-result "No persisted tool results found under artifact://."))
         (signal 'mevedel-resource-unavailable
                 (list "Internal resource error: artifact file reached discovery execution"))))
      ('mevedel
       (if (null components)
           (mevedel-resource--directory-list-result 'mevedel root)
         (signal 'mevedel-resource-unavailable
                 (list "Internal resource error: documentation file reached discovery execution"))))
      ('skill
       (if (null (plist-get data :source-file))
           (mevedel-resource--skill-list-result session context)
         (signal 'mevedel-resource-unavailable
                 (list "Internal resource error: skill file reached discovery execution"))))
      ('agent
       (if (null components)
           (mevedel-resource--agent-list-result session)
         (mevedel-resource--agent-read
          (plist-get data :record) parsed)))
      ('history
       (cond
        ((equal (car components) "saved")
         (list :history-workspace (mevedel-resource--workspace context session)
               :history-components components))
        ((null components)
         (concat (mevedel-resource--agent-list-result session t)
                 (when (mevedel-resource--workspace context session)
                   "\nhistory://saved\tSaved workspace conversations (Read, Glob, Grep)")))
        (t
         (let ((text (mevedel-resource--history-read
                      (plist-get data :record) session)))
           (if (eq operation 'grep)
               (list :resource-search-documents
                     (list (cons (car (last components)) text)))
             text)))))
      ((and 'memory (guard (equal (car components) "journal")))
       (let* ((components (cdr components))
              (workspace (mevedel-resource--workspace context session))
              (workspace-root (and workspace (mevedel-workspace-root workspace))))
         (unless workspace-root
           (signal 'mevedel-resource-unavailable '("Journal requires a workspace")))
         (condition-case err
             (let ((entries (if components
                                (list (mevedel-journal-store-read
                                       workspace-root (car components)))
                              (mevedel-journal-store-entries workspace-root))))
               (setq entries (seq-filter #'mevedel-journal-store-recall-p entries))
               (when (and components (null entries))
                 (signal 'mevedel-journal-store-invalid
                         '("Journal entry has expired from ordinary recall")))
               (if (eq operation 'read)
                   (if components
                       (plist-get (car entries) :text)
                     (if entries
                         (mapconcat
                          (lambda (entry)
                            (concat "memory://journal/"
                                    (mevedel-resource-encode-component
                                     (plist-get entry :file))))
                          entries "\n")
                       "No published journal entries"))
                 (list :resource-search-documents
                       (mapcar (lambda (entry)
                                 (cons (plist-get entry :file) (plist-get entry :text)))
                               entries))))
           ((file-missing mevedel-session-control-fs-absent)
            (signal 'mevedel-resource-unavailable
                    '("Journal entry not found; Read memory://journal/ to discover published entries")))
           (mevedel-journal-store-invalid
            (signal 'mevedel-resource-unavailable
                    (list (format "Invalid journal entry: %s"
                                  (car (last (cdr err)))))))
           (file-error
            (signal 'mevedel-resource-unavailable
                    '("Journal storage could not be read; check workspace storage access")))
           (error
            (signal 'mevedel-resource-unavailable
                    (list (format "Journal read failed: %s"
                                  (mevedel-resource-error-message
                                   err (plist-get data :address) (list root)))))))))
      ('memory
       (if (equal components '("root"))
           (pcase operation
             ('read
              (if (mevedel-resource--memory-roots context session)
                  (mevedel-system--memory-content
                   (mevedel-resource--workspace context session))
                "No memory roots configured."))
             ((or 'glob 'grep)
              (list :resource-search-roots
                    (mevedel-resource--memory-search-roots
                     context session)
                    :empty-result
                    (if (mevedel-resource--memory-roots context session)
                        "No memory files found under memory://root."
                      "No memory roots configured."))))
         (signal 'mevedel-resource-unavailable
                 (list "Internal resource error: memory file reached discovery execution"))))
      ('shared
       (if (and (null components) (eq operation 'read))
           (mevedel-resource--shared-list-result session)
         ;; Everything else is computed by the workspace's editing host, which
         ;; answers asynchronously; Read and Grep fetch it themselves.
         (list :shared-view components)))
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
                 (list "Internal resource error: unknown provider"))))))

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
           ;; ApplyPatch may be the first operation that creates the target.
           ;; Keep lexical containment as the authority while it is absent,
           ;; and prove canonical containment whenever both sides exist.
           (or (not (and (file-directory-p root) (file-exists-p path)))
               (let* ((true-root (file-name-as-directory
                                  (file-truename root)))
                      (true-path (file-truename path)))
                 (or (equal (directory-file-name true-root)
                            (directory-file-name true-path))
                     ;; `file-in-directory-p' compares root attributes twice;
                     ;; an unrelated sibling write can change its timestamps.
                     (string-prefix-p true-root true-path))))))))

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
     ((eq scheme 'work)
      (pcase-let ((`(,owner . ,relative) (mevedel-resource--work-location components context session)))
        (setq root owner
              physical (mevedel-resource--safe-path owner relative))))
     ((eq scheme 'artifact)
      (setq root (mevedel-resource--root scheme session)
            physical (mevedel-resource--safe-path root components))
      (when (and (eq scheme 'artifact)
                 (member ".mevedel-pending-executions" components))
        (setq data (plist-put data :unavailable-p t))))
     ((eq scheme 'mevedel)
      (setq root (mevedel-resource--root scheme nil)
            physical (mevedel-resource--safe-path root components))
      (unless (mevedel-resource--mevedel-path-available-p
               root physical components operation)
        (setq data (plist-put data :unavailable-p t))))
     ((eq scheme 'skill)
      (unless (plist-get data :dynamic-p)
        (let ((skill (mevedel-resource--skill-for-digest
                      (plist-get data :source-key) session context)))
          (if (not skill)
              (setq data (plist-put data :unavailable-p t))
            (let ((source-file (mevedel-skill-source-file skill)))
              (setq root (mevedel-resource--skill-root skill)
                    data (plist-put data :source-file source-file)
                    physical
                    (if (memq operation '(glob grep))
                        (mevedel-resource--safe-path root components)
                      (mevedel-resource--skill-physical-path
                       skill root components)))
              (unless (and (file-regular-p source-file)
                           (file-directory-p root)
                           (if (eq operation 'read)
                               (file-regular-p physical)
                             (file-exists-p physical)))
                (setq data (plist-put data :unavailable-p t))))))))
     ((and (eq scheme 'history) (equal (car components) "saved"))
      (unless (mevedel-resource--workspace context session)
        (setq data (plist-put data :unavailable-p t))))
     ((and (eq scheme 'history) (equal components '("root")))
      (unless (and session
                   (buffer-live-p (mevedel-session-root-buffer session)))
        (setq data (plist-put data :unavailable-p t))))
     ((memq scheme '(agent history))
      (when components
        (let ((record
               (and (equal (car components) "root")
                    (mevedel-resource--agent-record
                     (concat "/" (string-join components "/")) session))))
          (setq data (plist-put data :record record))
          (unless record
            (setq data (plist-put data :unavailable-p t))))))
     ((and (eq scheme 'memory) (equal (car components) "journal"))
      (if-let* ((workspace (mevedel-resource--workspace context session)))
          (progn
            (setq root (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
            (mevedel-resource--safe-path root (cdr components)))
        (setq data (plist-put data :unavailable-p t))))
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
                  data (plist-put data :memory-root memory-root))))))
     ((eq scheme 'shared)
      (unless (mevedel-resource--shared-available-p components session)
        (setq data (plist-put data :unavailable-p t)))))
    (when (and (eq operation 'apply-patch)
               (or (eq scheme 'memory)
                   (and (eq scheme 'work) (mevedel-resource--shared-work-p components)))
               (not (equal root (plist-get data :root))))
      (signal 'mevedel-resource-error
              (list (format "Writable resource root changed after preparation: %s; retry the patch against the current workspace"
                            (plist-get data :address)))))
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
                       :address (plist-get parsed :canonical)
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
                  (and (eq operation 'grep) (eq scheme 'shared))
                  (and (memq operation '(glob grep))
                       (or (memq scheme '(work artifact skill memory mevedel))
                           (and (eq scheme 'history)
                                (or (equal (car components) "saved")
                                    (and (eq operation 'grep)
                                         (equal (car components) "root"))))))
                  (and (eq operation 'apply-patch)
                       (memq scheme '(work memory))))
        (signal 'mevedel-resource-error
                (list (format "%s does not support %s:// resources"
                              (if (eq operation 'apply-patch) "ApplyPatch"
                                (capitalize (symbol-name operation))) scheme))))
      (when (and (eq operation 'apply-patch) (eq scheme 'memory)
                 (equal (car components) "journal"))
        (signal 'mevedel-resource-error '("Journal evidence is read-only")))
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
      (when (and (eq operation 'apply-patch)
                 (or (and (eq scheme 'memory) (< (length components) 2))
                     (and (eq scheme 'work) (mevedel-resource--shared-work-p components)
                          (null (cdr components)))))
        (signal 'mevedel-resource-error '("Patch targets must name a file descendant")))
      (cond
       ((eq scheme 'work)
        (pcase-let ((`(,owner . ,relative) (mevedel-resource--work-location components context session)))
          (setq root owner
                physical (mevedel-resource--safe-path owner relative)
                logical-p (or (null components)
                              (equal components '("shared"))))))
       ((eq scheme 'artifact)
        (setq root (mevedel-resource--root scheme session)
              physical (and root
                            (mevedel-resource--safe-path root components))
              logical-p (null components))
        (when (and (eq scheme 'artifact)
                   (member ".mevedel-pending-executions" components))
          (setq data (plist-put data :unavailable-p t))))
       ((eq scheme 'mevedel)
        (setq root (mevedel-resource--root scheme nil)
              physical (and root
                            (mevedel-resource--safe-path root components))
              logical-p (and (null components)
                             (eq operation 'read)))
        (unless (mevedel-resource--mevedel-path-available-p
                 root physical components operation)
          (setq data (plist-put data :unavailable-p t))))
       ((eq scheme 'skill)
        (if (plist-get parsed :dynamic-p)
            (setq logical-p t)
          (let* ((alias-p (eq (plist-get parsed :locator-class) 'alias))
                 (skill
                  (if alias-p
                      (mevedel-resource--skill-for-alias
                       parsed session context)
                    (mevedel-resource--skill-for-digest
                     (plist-get parsed :source-key) session context))))
            (if (not skill)
                (setq data (plist-put data :unavailable-p t))
              (let* ((source-file (mevedel-skill-source-file skill))
                     (source-key (mevedel-resource-skill-digest source-file))
                     (exact-address
                      (mevedel-resource--skill-address skill components)))
                (setq root (mevedel-resource--skill-root skill)
                    data (plist-put data :source-key source-key)
                    data (plist-put
                          data :source-file
                          source-file)
                    data (plist-put
                          data :exact-address exact-address))
                (if (memq operation '(glob grep))
                    (setq physical
                          (mevedel-resource--safe-path root components)
                          logical-p nil)
                  (setq physical
                        (mevedel-resource--skill-physical-path
                         skill root components))))))))
       ((memq scheme '(agent history))
        (setq logical-p t)
        (cond
         ((and (eq scheme 'history) (equal (car components) "saved"))
          (unless workspace (setq data (plist-put data :unavailable-p t))))
         ((null components))
         ((and (eq scheme 'history) (equal components '("root")))
          (unless (and session
                       (buffer-live-p (mevedel-session-root-buffer session)))
            (setq data (plist-put data :unavailable-p t))))
         (t
          (let ((record
                 (and (equal (car components) "root")
                      (mevedel-resource--agent-record
                       (concat "/" (string-join components "/")) session))))
            (setq data (plist-put data :record record))
            (unless record
              (setq data (plist-put data :unavailable-p t)))))))
       ((and (eq scheme 'memory) (equal (car components) "journal"))
        (setq logical-p t)
        (when workspace
          (setq root (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
          (mevedel-resource--safe-path root (cdr components))))
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
        (setq logical-p t))
       ((eq scheme 'shared)
        (unless (mevedel-resource--shared-shape-p components)
          (signal 'mevedel-resource-error
                  (list "Unknown shared:// address; Read shared:// for this project's items")))
        (setq logical-p t)
        (unless (mevedel-resource--shared-available-p components session)
          (setq data (plist-put data :unavailable-p t)))))
      (setq data (plist-put data :root root))
      (setq data (plist-put data :physical-path physical))
      (setq data (plist-put data :logical-p logical-p))
      (let ((attempt (make-symbol "mevedel-resource-attempt-")))
        (puthash attempt data mevedel-resource--attempt-table)
        (when-let* ((cell (or (plist-get context :resource-attempts-cell)
                             mevedel-resource-attempts-cell)))
          (when (consp cell)
            (setcar cell (cons attempt (car cell)))))
        attempt))
      (mevedel-resource-error
       (signal 'mevedel-resource-error
               (list (mevedel-resource-error-message err address)))))))

(defun mevedel-resource--unavailable-reason (data)
  "Return the actionable availability failure for refreshed DATA, or nil."
  (let ((scheme (plist-get data :scheme))
        (session (plist-get data :session))
        (components (plist-get data :components))
        (root (plist-get data :root))
        (path (plist-get data :physical-path))
        (unavailable (plist-get data :unavailable-p)))
    (cond
     ((and (eq scheme 'work) (mevedel-resource--shared-work-p components))
      (unless root "Shared working files require a workspace"))
     ((and (eq scheme 'history) (equal (car components) "saved"))
      (when unavailable "Saved history requires a workspace"))
     ((and (memq scheme '(work artifact agent history shared)) (null session))
      (format "%s resources require a session"
              (if (eq scheme 'work) "Session working file"
                (capitalize (symbol-name scheme)))))
     ((not unavailable) nil)
     ((eq scheme 'artifact)
      "Artifact is not published; Read artifact:// to discover available results")
     ((eq scheme 'mevedel)
      (cond
       ((or (null root) (not (file-directory-p root))
            (file-symlink-p (directory-file-name root)))
        "Installed documentation is missing or its root is invalid")
       ((not (file-exists-p path)) "File not found")
       ((file-directory-p path)
        "Read requires a Markdown document; use Glob or Grep for documentation directories")
       (t "Only packaged Markdown documents are readable; Read mevedel:// to discover documents")))
     ((eq scheme 'skill)
      (cond
       ((or (null path) (null (plist-get data :source-file)))
        "Skill not found or disabled; Read skill:// to discover available skills")
       ((not (file-regular-p (plist-get data :source-file)))
        "Skill source file is missing; Read skill:// to discover available skills")
       ((not (file-exists-p path)) "File not found")
       (t "Read requires a skill file; use Glob or Grep for package directories")))
     ((and (eq scheme 'history) (equal components '("root")))
      "Root conversation is not open in this session")
     ((memq scheme '(agent history))
      (format "Agent not found in this session; Read %s:// to discover available %s"
              scheme (if (eq scheme 'agent) "agents" "histories")))
     ((and (eq scheme 'memory) (equal (car components) "journal"))
      "Journal resources require a workspace")
     ((eq scheme 'memory)
      "Memory root is not configured; Read memory://root to discover configured roots")
     ((eq scheme 'shared)
      "Shared item not found; Read shared:// to list this project's whiteboards and documents")
     (t "Internal resource error: availability failure has no reason"))))

(defun mevedel-resource-attempt-address (attempt)
  "Return ATTEMPT's authored address."
  (plist-get (gethash attempt mevedel-resource--attempt-table) :address))

(defun mevedel-resource-attempt-write-check (attempt)
  "Return a freshness check for ATTEMPT's workspace or memory write target.
The closure survives attempt consumption and is checked through patch review
and commit.  It does not grant authority or read file content."
  (let ((data (copy-sequence (gethash attempt mevedel-resource--attempt-table))))
    (when (and (eq (plist-get data :operation) 'apply-patch)
               (not (mevedel-resource-session-work-p (plist-get data :address))))
      (let* ((root (plist-get data :root))
             (remote-file-name-inhibit-cache t)
             (identity (and root (file-truename root))))
        (lambda ()
          (let* ((remote-file-name-inhibit-cache t)
                 (fresh (mevedel-resource--refresh-data (copy-sequence data))))
            (unless (and root
                         (not (plist-get fresh :unavailable-p))
                         (equal identity (file-truename root)))
              (signal 'mevedel-resource-error
                      (list (format "Writable resource target changed during review: %s; review a fresh patch"
                                    (plist-get data :address)))))
            t))))))

(defun mevedel-resource-attempt-write-path (attempt)
  "Return ATTEMPT's prepared backing path for filesystem write policy.
Session-owned scratch keeps its address-based permission treatment."
  (let ((data (gethash attempt mevedel-resource--attempt-table)))
    (when (and (eq (plist-get data :operation) 'apply-patch)
               (not (mevedel-resource-session-work-p (plist-get data :address))))
      (or (plist-get data :physical-path)
          (signal 'mevedel-resource-unavailable
                  (list (mevedel-resource-error-message
                         (list 'mevedel-resource-unavailable
                               (or (mevedel-resource--unavailable-reason
                                    (mevedel-resource--refresh-data (copy-sequence data)))
                                   "Writable resource owner is unavailable"))
                         (plist-get data :address))))))))

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
        (condition-case err
            (progn
              (unless (plist-get data :resource-p)
                (signal 'mevedel-resource-error
                        (list "Internal resource error: invalid execution attempt")))
              (setq data (mevedel-resource--refresh-data data))
              (when-let* ((reason (mevedel-resource--unavailable-reason data)))
                (signal 'mevedel-resource-unavailable
                        (list reason)))
              (when (and (plist-get data :logical-p)
                         (memq (plist-get data :scheme) '(work artifact))
                         (plist-get data :root)
                         (file-exists-p (plist-get data :root))
                         (not (file-directory-p (plist-get data :root))))
                (signal 'mevedel-resource-unavailable
                        '("Resource storage root is not a directory")))
              (if (plist-get data :logical-p)
                  (let* ((result (mevedel-resource--execute-logical data options))
                         (descriptor (list :virtual t
                                           :result (if (and (listp result)
                                                            (plist-member result :resource-search-roots)
                                                            (null (plist-get result :resource-search-roots)))
                                                       (plist-get result :empty-result)
                                                     result)
                                           :address (plist-get data :address)
                                           :scheme (plist-get data :scheme)
                                           :operation (plist-get data :operation))))
                    (when (and (listp result)
                               (plist-member result :resource-search-roots))
                      (if (plist-get result :resource-search-roots)
                          (setq descriptor
                                (plist-put descriptor :resource-search-roots
                                           (plist-get result :resource-search-roots)))
                        (setq descriptor (plist-put descriptor :render-data '(:count 0)))))
                    (when (and (listp result) (plist-member result :resource-search-documents))
                      (setq descriptor (append descriptor result)))
                    (when (and (listp result)
                               (or (plist-member result :history-workspace)
                                   (plist-member result :shared-view)))
                      (setq descriptor (append descriptor result)))
                    (if executor
                        (funcall executor descriptor (plist-get data :address))
                      descriptor))
                (unless (functionp executor)
                  (signal 'mevedel-resource-error
                          (list "Internal resource error: file-backed resource has no executor")))
                (funcall executor
                         (plist-get data :physical-path)
                         (plist-get data :address))))
          (error
           (signal (car err)
                   (list (mevedel-resource-error-message
                          err (plist-get data :address)
                          (list (plist-get data :physical-path)
                                (plist-get data :root)
                                (when-let* ((session (plist-get data :session)))
                                  (mevedel-session-save-path session))
                                (when (eq (plist-get data :scheme) 'work)
                                  (mevedel-resource-work-shared-directory
                                   (plist-get data :workspace)))))))))
      (remhash attempt mevedel-resource--attempt-table))))

(defun mevedel-resource-discard-attempts (attempts)
  "Discard opaque resource ATTEMPTS that will not be executed."
  (dolist (attempt attempts)
    (remhash attempt mevedel-resource--attempt-table))
  nil)

(defun mevedel-resource-visit-path (address &optional context)
  "Return the file or directory ADDRESS names in CONTEXT, or nil.
Logical resources -- listings, agents, history, MCP and shared items --
have no backing file and return nil, as does an unavailable owner."
  (let ((attempt (mevedel-resource-prepare 'read address context)))
    (unwind-protect
        (let ((data (gethash attempt mevedel-resource--attempt-table)))
          (and data
               (not (plist-get data :unavailable-p))
               (not (plist-get data :logical-p))
               (plist-get data :physical-path)))
      (when attempt (mevedel-resource-discard-attempts (list attempt))))))

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
