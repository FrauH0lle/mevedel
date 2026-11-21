;;; niwa-resource.el -- Canonical resource addresses -*- lexical-binding: t -*-

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

;; `niwa-utilities'
(declare-function niwa-collapse-leading-slashes "niwa-utilities" (path))
(autoload 'niwa-collapse-leading-slashes "niwa-utilities")
(declare-function niwa-file-truename "niwa-utilities" (filename))
(autoload 'niwa-file-truename "niwa-utilities")

;; `cl-lib'
(declare-function cl-count-if "cl-lib" (predicate sequence &rest args))
(declare-function cl-find-if "cl-lib" (predicate sequence &rest args))
(declare-function cl-remove-if-not "cl-lib" (predicate sequence &rest args))

;; `mcp'
(declare-function mcp-hub-get-servers "mcp-hub" ())
(declare-function mcp-read-resource "mcp" (connection uri))
(defvar mcp-server-connections)

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-activity-p
                  "niwa-agent-control" (activity))
(declare-function niwa-agent-control-list-agents
                  "niwa-agent-control" (session &optional path-prefix))
(declare-function niwa-agent-control-settled-result
                  "niwa-agent-control" (record))
(declare-function niwa-agent-record-activity
                  "niwa-agent-control" (record) t)
(declare-function niwa-agent-record-conversation-buffer
                  "niwa-agent-control" (record) t)
(declare-function niwa-agent-record-conversation-location
                  "niwa-agent-control" (record) t)
(declare-function niwa-agent-record-path
                  "niwa-agent-control" (record) t)
(autoload 'niwa-agent-control-active-activity-p "niwa-agent-control")
(autoload 'niwa-agent-control-list-agents "niwa-agent-control")
(autoload 'niwa-agent-control-settled-result "niwa-agent-control")
(autoload 'niwa-agent-record-activity "niwa-agent-control")
(autoload 'niwa-agent-record-conversation-buffer "niwa-agent-control")
(autoload 'niwa-agent-record-conversation-location "niwa-agent-control")
(autoload 'niwa-agent-record-path "niwa-agent-control")

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-project-history
                  "niwa-agent-conversation" (buffer &optional session))
(autoload 'niwa-agent-conversation-project-history
  "niwa-agent-conversation")

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-ensure-conversation
                  "niwa-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'niwa-agent-persistence-ensure-conversation
  "niwa-agent-persistence")

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-catalog "niwa-artifact-backend"
                  (workspace &optional detail))
(declare-function niwa-artifact-backend-present-p "niwa-artifact-backend" (workspace id))
(declare-function niwa-artifact-backend-registered-p "niwa-artifact-backend" (workspace id))
(autoload 'niwa-artifact-backend-catalog "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-present-p "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-registered-p "niwa-artifact-backend")

;; `niwa-execution'
(declare-function niwa-execution-list-user "niwa-execution" (session))

;; `niwa-execution-spool'
(declare-function niwa-execution-spool-logical-name "niwa-execution-spool" (path))
(declare-function niwa-execution-spool-physical-path "niwa-execution-spool" (path))
(declare-function niwa-execution-spool-private-p "niwa-execution-spool" (path))
(declare-function niwa-execution-spool-with-path "niwa-execution-spool"
                  (path operation callback executor &optional max-lines))
(autoload 'niwa-execution-spool-logical-name "niwa-execution-spool")
(autoload 'niwa-execution-spool-physical-path "niwa-execution-spool")
(autoload 'niwa-execution-spool-private-p "niwa-execution-spool")
(autoload 'niwa-execution-spool-with-path "niwa-execution-spool")

;; `niwa-journal-index'
(declare-function niwa-journal-index-entries "niwa-journal-index" (workspace &optional cached-only))
(autoload 'niwa-journal-index-entries "niwa-journal-index")

;; `niwa-journal-store'
(declare-function niwa-journal-store-directory "niwa-journal-store" (root))
(declare-function niwa-journal-store-entries "niwa-journal-store" (root))
(declare-function niwa-journal-store-file-name-p "niwa-journal-store" (name))
(declare-function niwa-journal-store-read "niwa-journal-store" (root file))
(declare-function niwa-journal-store-recall-p "niwa-journal-store" (entry &optional now))
(autoload 'niwa-journal-store-directory "niwa-journal-store")
(autoload 'niwa-journal-store-entries "niwa-journal-store")
(autoload 'niwa-journal-store-file-name-p "niwa-journal-store")
(autoload 'niwa-journal-store-read "niwa-journal-store")
(autoload 'niwa-journal-store-recall-p "niwa-journal-store")

;; `niwa-note-resource'
(declare-function niwa-note-resource-execute "niwa-note-resource" (operation components session))
(autoload 'niwa-note-resource-execute "niwa-note-resource")

;; `niwa-pipeline'
(declare-function niwa-pipeline-handler-resumable "niwa-pipeline" (function))
(autoload 'niwa-pipeline-handler-resumable "niwa-pipeline")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-segment-number
                  "niwa-session-artifacts" (logical))
(autoload 'niwa-session-artifacts-segment-number "niwa-session-artifacts")

;; `niwa-session-publication'
(declare-function niwa-session-publication-uncommitted-artifact
                  "niwa-session-publication" (session logical))
(autoload 'niwa-session-publication-uncommitted-artifact "niwa-session-publication")

;; `niwa-skills-core'
(declare-function niwa-skill-description "niwa-skills-core" (skill) t)
(declare-function niwa-skill-name "niwa-skills-core" (skill) t)
(declare-function niwa-skill-plugin-name "niwa-skills-core" (skill) t)
(declare-function niwa-skill-raw-name "niwa-skills-core" (skill) t)
(declare-function niwa-skill-source "niwa-skills-core" (skill) t)
(declare-function niwa-skill-source-dir "niwa-skills-core" (skill) t)
(declare-function niwa-skill-source-family "niwa-skills-core" (skill) t)
(declare-function niwa-skill-source-file "niwa-skills-core" (skill) t)
(declare-function niwa-skills-skill-enabled-p "niwa-skills-core" (skill))
(declare-function niwa-skills-source-key "niwa-skills-core" (source-file))
(declare-function niwa-skills-scan "niwa-skills-core"
                  (&optional workspace-root dirs workspace))
(autoload 'niwa-skills-scan "niwa-skills-core")

;; `niwa-structs'
(declare-function niwa-agent-path-p "niwa-structs" (path))
(declare-function niwa-session-agent-registry "niwa-structs" (cl-x) t)
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-skills "niwa-structs" (cl-x) t)
(declare-function niwa-session-attached-artifacts "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa--workspace)

;; `niwa-system'
(declare-function niwa-system--memory-content "niwa-system"
                  (&optional workspace))
(declare-function niwa-system--memory-roots "niwa-system"
                  (&optional workspace))
(autoload 'niwa-system--memory-content "niwa-system")
(autoload 'niwa-system--memory-roots "niwa-system")

;; `niwa-utilities'
(declare-function niwa--transcript-org-mode "niwa-utilities" ())
(declare-function niwa-library-source-directory "niwa-utilities" (file))
(autoload 'niwa--transcript-org-mode "niwa-utilities")
(autoload 'niwa-library-source-directory "niwa-utilities")

;; `niwa-working-note-files'
(declare-function niwa-working-note-files-listing "niwa-working-note-files" (directory address))
(autoload 'niwa-working-note-files-listing "niwa-working-note-files")

(defconst niwa-resource-supported-schemes
  '(work artifact skill agent history memory mcp shared niwa notes)
  "Closed set of resource address schemes understood by niwa.")

(defconst niwa-resource--unreserved
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
  "RFC 3986 unreserved bytes.")

(defconst niwa-resource--skill-alias-sources
  '(("local-niwa" . local-niwa)
    ("local-agents" . local-agents)
    ("global-niwa" . global-niwa)
    ("global-agents" . global-agents)
    ("bundled" . bundled)
    ("managed" . managed)
    ("plugin" . plugin))
  "Closed readable source names accepted by `skill://' aliases.")

(defconst niwa-resource--memory-alias-keys
  '("local-niwa" "local-agents" "global-niwa" "global-agents")
  "Closed readable root keys accepted by `memory://' aliases.")

(defvar niwa-resource--source-dir
  (niwa-library-source-directory
   (or load-file-name buffer-file-name (locate-library "niwa-resource")))
  "Canonical package directory containing Niwa's documentation.")

(defvar niwa-resource-current-attempts nil
  "Dynamically bound resource attempts for the active pipeline handler.
The pipeline binds this around each handler; it is the module's
dynamic seam, not private state.")

(defvar niwa-resource--attempt-table (make-hash-table :test #'eq)
  "Private table mapping opaque attempt tokens to resolver data.")

(defvar niwa-resource-attempts-cell nil
  "Dynamically bound cell collecting attempts for one pipeline run.")

(define-error 'niwa-resource-error "Resource address error")
(define-error 'niwa-resource-unavailable "Resource unavailable")

(defun niwa-resource-error-message (failure &optional address private-paths)
  "Format FAILURE once, naming ADDRESS and hiding PRIVATE-PATHS.
Resource conditions already contain a useful message; omit their redundant
condition labels.  Other conditions retain their ordinary diagnostic text."
  (let ((message (if (memq (car failure)
                           '(niwa-resource-error niwa-resource-unavailable))
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

(defun niwa-resource--control-character-p (character)
  "Return non-nil when CHARACTER is a disallowed control character."
  (or (< character #x20) (= character #x7f)))

(defun niwa-resource--has-control-character-p (value)
  "Return non-nil when VALUE contains a disallowed control character."
  (let ((index 0)
        found)
    (while (and (< index (length value)) (not found))
      (setq found (niwa-resource--control-character-p
                   (aref value index)))
      (setq index (1+ index)))
    found))

(defun niwa-resource--lowercase-digest-p (value)
  "Return non-nil when VALUE is a lowercase SHA-256 digest."
  (let ((case-fold-search nil))
    (and (stringp value)
         (string-match-p "\\`[0-9a-f]\\{64\\}\\'" value))))

(defun niwa-resource-supported-scheme-p (scheme)
  "Return the scheme symbol SCHEME names, or nil.
SCHEME is a name or a symbol; a name is looked up, never interned."
  (let ((symbol (if (stringp scheme)
                    (intern-soft (downcase scheme))
                  scheme)))
    (and (memq symbol niwa-resource-supported-schemes) symbol)))

(defun niwa-resource--scheme-prefix (value)
  "Return the scheme name before `://` in VALUE, or nil.
Any URI-shaped prefix answers, not only a supported one: that is what
makes an unknown scheme a rejected address rather than a relative
filesystem path."
  (when (and (stringp value)
             (string-match "\\`\\([[:alnum:]][[:alnum:]+.-]*\\)://" value))
    (downcase (match-string 1 value))))

(defun niwa-resource-address-like-p (value)
  "Return non-nil when VALUE has a URI-like `scheme://` prefix."
  (and (stringp value) (niwa-resource--scheme-prefix value)))

(defun niwa-resource-address-p (value)
  "Return non-nil when VALUE starts with a supported resource prefix."
  (and (stringp value)
       (niwa-resource-supported-scheme-p
        (niwa-resource--scheme-prefix value))))

(defun niwa-resource-normalize-file-path (value &optional directory)
  "Return ordinary filesystem VALUE as an absolute locator.

Environment references are substituted before expansion.  DIRECTORY is the
base directory for relative values; malformed substitutions leave VALUE
literal and are expanded in that same base directory."
  (if (or (not (stringp value)) (string-empty-p value))
      value
    (niwa-collapse-leading-slashes
     (expand-file-name
      (condition-case nil
          (substitute-in-file-name value)
        (error value))
      directory))))

(defun niwa-resource-encode-component (value)
  "Return VALUE encoded as one canonical RFC 3986 UTF-8 component."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value)))
  (let ((bytes (encode-coding-string value 'utf-8 t))
        (result nil))
    (dotimes (index (length bytes))
      (let ((byte (aref bytes index)))
        (if (string-search (char-to-string byte) niwa-resource--unreserved)
            (push (char-to-string byte) result)
          (push (format "%%%02X" byte) result))))
    (apply #'concat (nreverse result))))

(defun niwa-resource--decode-component (raw &optional allow-separator)
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
                (signal 'niwa-resource-error
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
        (signal 'niwa-resource-error '("Invalid UTF-8 in resource address")))
      (when (or (string-empty-p decoded)
                (niwa-resource--has-control-character-p decoded)
                (and (not allow-separator)
                     (member decoded '("." "..")))
                (and (not allow-separator) (string-match-p "/" decoded)))
        (signal 'niwa-resource-error
                (list "Unsafe resource path component")))
      decoded)))

(defun niwa-resource--parse-components (tail)
  "Parse slash-separated path TAIL into decoded canonical components."
  (if (string-empty-p tail)
      nil
    (let ((raw-components (split-string tail "/" nil)))
      (when (member "" raw-components)
        (signal 'niwa-resource-error
                (list "Empty resource path component")))
      (mapcar #'niwa-resource--decode-component raw-components))))

(defun niwa-resource--canonical-components (components)
  "Return canonical slash-separated encoding for COMPONENTS."
  (mapconcat #'niwa-resource-encode-component components "/"))

(defun niwa-resource--decode-json-pointer (fragment)
  "Validate decoded JSON Pointer FRAGMENT and return its tokens.

FRAGMENT is the decoded URI fragment, not the raw address spelling.  The
empty fragment selects the complete JSON value."
  (when (string-match-p "[^\u0000-\U0010ffff]" fragment)
    (signal 'niwa-resource-error '("Invalid UTF-8 in JSON Pointer")))
  (when (niwa-resource--has-control-character-p fragment)
    (signal 'niwa-resource-error
            (list "JSON Pointer fragment contains a control character")))
  (unless (or (string-empty-p fragment)
              (string-prefix-p "/" fragment))
    (signal 'niwa-resource-error
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
                     (signal 'niwa-resource-error
                             (list "Invalid JSON Pointer escape")))
                   (push (if (= (aref token (1+ index)) ?0) ?~ ?/)
                         decoded)
                   (setq index (+ index 2)))
               (push character decoded)
               (setq index (1+ index)))))
         (apply #'string (nreverse decoded))))
     (split-string (substring fragment 1) "/" nil))))

(defun niwa-resource--decode-fragment (raw)
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
                (signal 'niwa-resource-error
                        (list "Malformed percent escape in JSON Pointer")))
              (push (string-to-number (substring raw (1+ index) (+ index 3))
                                      16)
                    bytes)
              (setq index (+ index 3)))
          (when (or (>= character 128) (= character ??))
            (signal 'niwa-resource-error
                    (list "JSON Pointer fragment must be canonically encoded")))
          (push character bytes)
          (setq index (1+ index)))))
    (let* ((fragment
            (decode-coding-string
             (apply #'unibyte-string (nreverse bytes)) 'utf-8 t))
           (tokens (niwa-resource--decode-json-pointer fragment))
           (canonical
            (mapconcat
             (lambda (character)
               (if (or (= character ?/)
                       (string-search (char-to-string character)
                                      niwa-resource--unreserved))
                   (char-to-string character)
                 (niwa-resource-encode-component
                  (char-to-string character))))
             (string-to-list fragment) "")))
      (unless (equal raw canonical)
        (signal 'niwa-resource-error
                (list "Noncanonical JSON Pointer fragment")))
      (list :fragment fragment :raw canonical :tokens tokens))))

(defun niwa-resource--parse-skill-tail (tail)
  "Parse the skill-specific TAIL and return its locator fields."
  (if (string-empty-p tail)
      (list :components nil :name nil :source-key nil :dynamic-p t)
    (let* ((slash (string-match "/" tail))
           (head (if slash (substring tail 0 slash) tail))
           (alias-source
            (and (not (string-search "@" head))
                 (cdr (assoc (niwa-resource--decode-component head)
                             niwa-resource--skill-alias-sources)))))
      (if alias-source
          (let* ((parts (niwa-resource--parse-components tail))
                 (plugin-p (eq alias-source 'plugin))
                 (minimum (if plugin-p 3 2)))
            (when (< (length parts) minimum)
              (signal 'niwa-resource-error
                      (list "Skill alias is missing its skill name")))
            (list :components (nthcdr minimum parts)
                  :alias-source alias-source
                  :plugin-name (and plugin-p (cadr parts))
                  :raw-name (nth (1- minimum) parts)
                  :canonical-tail
                  (niwa-resource--canonical-components parts)
                  :locator-class 'alias
                  :dynamic-p nil))
        (let* ((path (and slash (substring tail (1+ slash))))
               (at (string-match "@" head)))
          (when (or (null at)
                    (= at 0)
                    (= (1+ at) (length head))
                    (string-match "@" (substring head (1+ at)))
                    (not (niwa-resource--lowercase-digest-p
                          (substring head (1+ at)))))
            (signal
             'niwa-resource-error
             (list "Skill address requires a name and lowercase source digest")))
          (let* ((name (niwa-resource--decode-component
                        (substring head 0 at)))
                 (source-key (substring head (1+ at)))
                 (components (if slash
                                 (niwa-resource--parse-components path)
                               nil)))
            (list :components components :name name :source-key source-key
                  :dynamic-p nil)))))))

(defun niwa-resource--parse-memory-tail (tail)
  "Parse the memory-specific TAIL and return its locator fields."
  (let ((components (niwa-resource--parse-components tail)))
    (cond
     ((equal (car components) "journal")
      (unless (or (null (cdr components))
                  (and (= (length components) 2)
                       (niwa-journal-store-file-name-p (cadr components))))
        (signal 'niwa-resource-error
                '("Journal addresses use memory://journal/ or name one public entry")))
      (list :components components :dynamic-p (null (cdr components))
            :locator-class (unless (null (cdr components)) 'workspace-relative)))
     ((equal components '("root"))
      (list :components components :dynamic-p t))
     ((and (>= (length components) 2)
           (or (niwa-resource--lowercase-digest-p (car components))
               (member (car components)
                       niwa-resource--memory-alias-keys)))
      (list :components components :dynamic-p nil))
     (t
      (signal 'niwa-resource-error
              (list "Memory address requires 'root', 'journal/', or a root key and path"))))))

(defun niwa-resource--parse-agent-history-components (components)
  "Validate retained-agent COMPONENTS for agent or history resources."
  (if (null components)
      (list :components nil :dynamic-p t)
    (unless (and (> (length components) 1)
                 (equal (car components) "root"))
      (signal 'niwa-resource-error
              (list "Agent and history addresses require a retained /root path")))
    (unless (niwa-agent-path-p
             (concat "/" (string-join components "/")))
      (signal 'niwa-resource-error
              (list "Agent and history addresses require a canonical agent path")))
    (list :components components :dynamic-p nil)))

(defun niwa-resource--parse-mcp-tail (tail)
  "Parse the MCP-specific TAIL and return its locator fields."
  (if (string-empty-p tail)
      (list :components nil :dynamic-p t)
    (let ((raw-components (split-string tail "/" nil)))
      (when (or (member "" raw-components) (> (length raw-components) 2))
        (signal 'niwa-resource-error
                (list "MCP address has an invalid component count")))
      (list :components
            (mapcar (lambda (raw) (niwa-resource--decode-component raw t))
                    raw-components)
            :dynamic-p (= (length raw-components) 1)))))

(defun niwa-resource--session (context)
  "Return the owning session from CONTEXT, or nil."
  (or (plist-get context :session)
      (and (boundp 'niwa--session) niwa--session)))

(defun niwa-resource--workspace (context session)
  "Return the workspace represented by CONTEXT and SESSION."
  (or (plist-get context :workspace)
      (and session (niwa-session-workspace session))))

(defun niwa-resource--digest (value)
  "Return the lowercase SHA-256 digest of canonical locator VALUE."
  (secure-hash 'sha256 value))

(defun niwa-resource--skill-source-key (source-file)
  "Return the canonical skill source key for SOURCE-FILE."
  (or (and (fboundp 'niwa-skills-source-key)
           (niwa-skills-source-key source-file))
      (concat "file:" (file-truename source-file))))

(defun niwa-resource-skill-digest (source-file)
  "Return the lowercase address digest for skill SOURCE-FILE."
  (niwa-resource--digest
   (niwa-resource--skill-source-key source-file)))

(defun niwa-resource--memory-root-key (root)
  "Return the stable digest key for memory ROOT metadata."
  (niwa-resource--digest
   (concat "memory:" (file-name-as-directory
                       (file-truename (plist-get root :dir))))))

(defun niwa-resource-memory-root-key (root)
  "Return the stable address key for memory ROOT metadata or directory."
  (niwa-resource--memory-root-key
   (if (listp root)
       root
     (list :dir root))))

(defun niwa-resource--memory-root-address-key (root roots)
  "Return ROOT's readable alias key, or its digest.

The alias is used only when it names exactly one root among ROOTS; the
digest remains the authority a readable key resolves to."
  (let ((alias (plist-get root :alias)))
    (if (and alias
             (member alias niwa-resource--memory-alias-keys)
             (= 1 (cl-count-if
                   (lambda (other) (equal alias (plist-get other :alias)))
                   roots)))
        alias
      (niwa-resource--memory-root-key root))))

(defun niwa-resource--skill-list (session context)
  "Return currently discoverable skills for SESSION and CONTEXT."
  (let* ((workspace (niwa-resource--workspace context session))
         (workspace-root (and workspace (niwa-workspace-root workspace)))
         (skills (and session (niwa-session-skills session))))
    (or skills
        (niwa-skills-scan workspace-root nil workspace))))

(defun niwa-resource--skill-for-digest (digest session context)
  "Return the skill whose exact source identity hashes to DIGEST."
  (cl-find-if
   (lambda (skill)
     (let ((source (ignore-errors
                     (niwa-skill-source-file skill))))
       (and source
            (equal digest (niwa-resource-skill-digest source))
            (or (not (fboundp 'niwa-skills-skill-enabled-p))
                (niwa-skills-skill-enabled-p skill)))))
   (niwa-resource--skill-list session context)))

(defun niwa-resource--skill-alias-source (skill)
  "Return SKILL's readable alias source, or nil when it has none."
  (pcase (niwa-skill-source skill)
    ('project
     (pcase (niwa-skill-source-family skill)
       ('niwa 'local-niwa)
       ('agents 'local-agents)))
    ('user
     (pcase (niwa-skill-source-family skill)
       ('niwa 'global-niwa)
       ('agents 'global-agents)))
    ((or 'bundled 'managed 'plugin)
     (niwa-skill-source skill))))

(defun niwa-resource--skill-alias-address (skill)
  "Return SKILL's readable alias address, or nil when incomplete."
  (when-let* ((source (niwa-resource--skill-alias-source skill))
              (raw-name (niwa-skill-raw-name skill))
              ((or (not (eq source 'plugin))
                   (niwa-skill-plugin-name skill))))
    (concat "skill://" (symbol-name source) "/"
            (if (eq source 'plugin)
                (concat
                 (niwa-resource-encode-component
                  (niwa-skill-plugin-name skill)) "/")
              "")
            (niwa-resource-encode-component raw-name))))

(defun niwa-resource--skill-for-alias (parsed session context)
  "Resolve PARSED readable skill alias uniquely in SESSION and CONTEXT."
  (let (matches)
    (dolist (skill (niwa-resource--skill-list session context))
      (when (and
             (eq (plist-get parsed :alias-source)
                 (niwa-resource--skill-alias-source skill))
             (equal (plist-get parsed :raw-name)
                    (niwa-skill-raw-name skill))
             (equal (plist-get parsed :plugin-name)
                    (niwa-skill-plugin-name skill))
             (or (not (fboundp 'niwa-skills-skill-enabled-p))
                 (niwa-skills-skill-enabled-p skill)))
        (push skill matches)))
    (cond
     ((null matches)
      (signal 'niwa-resource-error
              (list "Skill alias does not name an enabled skill; Read skill:// to discover available skills")))
     ((cdr matches)
      (signal 'niwa-resource-error
              (list "Skill alias is ambiguous; Read skill:// and use an exact source address")))
     (t (car matches)))))

(defun niwa-resource--skill-root (skill)
  "Return SKILL's package root, deriving it from its source when needed."
  (or (niwa-skill-source-dir skill)
      (when-let* ((source (niwa-skill-source-file skill)))
        (file-name-directory source))))

(defun niwa-resource--skill-address (skill &optional components)
  "Return the canonical exact address for SKILL and COMPONENTS."
  (concat
   (format "skill://%s@%s"
           (niwa-resource-encode-component
            (niwa-skill-name skill))
           (niwa-resource-skill-digest
            (niwa-skill-source-file skill)))
   (when components
     (concat "/" (niwa-resource--canonical-components components)))))

(defun niwa-resource--memory-roots (context session)
  "Return configured memory root metadata for CONTEXT and SESSION."
  (niwa-system--memory-roots
   (niwa-resource--workspace context session)))

(defun niwa-resource-completion-metadata (context &optional scheme)
  "Return resource-owned metadata for completion in CONTEXT and SCHEME.

The returned plist keeps scheme-specific lookup and path safety inside this
module.  Function-valued slots are intentionally opaque operations for the
completion consumer; they do not expose backing roots as candidates.  When
SCHEME is nil, include metadata for every scheme."
  (let* ((session (niwa-resource--session context))
         (skills (and (memq scheme '(nil skill))
                      session
                      (cl-remove-if-not
                       (lambda (skill)
                         (let ((source (niwa-skill-source-file skill)))
                           (and source
                                (or (null scheme)
                                    (not (file-remote-p source)))
                                (or (not (fboundp
                                          'niwa-skills-skill-enabled-p))
                                    (niwa-skills-skill-enabled-p skill)))))
                       (if scheme
                           (niwa-session-skills session)
                         (niwa-resource--skill-list session context)))))
         (agents (and (memq scheme '(nil agent history))
                      session
                      (niwa-agent-control-list-agents session)))
         (memory-roots (and (memq scheme '(nil memory))
                            session
                            (niwa-resource--workspace context session)
                            (niwa-resource--memory-roots context session)))
         (shared-items (and (memq scheme '(nil shared))
                            session
                            (niwa-artifact-backend-catalog
                             (niwa-session-workspace session) 'items)))
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
                            (niwa-resource--root 'work session)))
                 (and (memq scheme '(nil artifact))
                      (cons 'artifact
                            (niwa-resource--root 'artifact session)))
                 (and (memq scheme '(nil niwa))
                      (cons 'niwa
                            (niwa-resource--root 'niwa nil)))))
          :shared-root
          (when (memq scheme '(nil work))
            (niwa-resource-work-shared-directory
             (niwa-resource--workspace context session)))
          :decode-component #'niwa-resource--decode-component
          :saved-history-p
          (and (memq scheme '(nil history))
               (niwa-resource--workspace context session) t)
          :notes-p
          (and (memq scheme '(nil notes)) session
               (niwa-session-workspace session) t)
          :safe-path #'niwa-resource--safe-path
          :skills
          (delq nil
                (mapcar
                 (lambda (skill)
                   (when-let* ((address
                               (condition-case nil
                                   (niwa-resource--skill-address skill)
                                 (error nil))))
                     (list :skill skill
                           :address address
                           :alias
                           (niwa-resource--skill-alias-address skill))))
                 skills))
          :agents
          (mapcar
           (lambda (item)
             (let* ((path (plist-get item :path))
                    (record (niwa-resource--agent-record path session)))
               (list :item item :record record
                     :history-p
                     (if (equal path "/root")
                         (buffer-live-p (niwa-session-root-buffer session))
                       (and record
                            (or (buffer-live-p
                                 (niwa-agent-record-conversation-buffer record))
                                (niwa-agent-record-conversation-location
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
                             :key (niwa-resource--memory-root-address-key
                                   root memory-roots))))
                        memory-roots))
          :journal
          (when (memq scheme '(nil memory))
            (mapcar (lambda (entry) (list :file (plist-get entry :file)))
                    (niwa-journal-index-entries
                     (niwa-resource--workspace context session) scheme)))
          :shared-items shared-items
          :mcp-servers servers)))

(defun niwa-resource--memory-root-for-key (key context session)
  "Return the configured memory root whose digest or alias is KEY."
  (let ((roots (niwa-resource--memory-roots context session)))
    (if (niwa-resource--lowercase-digest-p key)
        (cl-find-if
         (lambda (root)
           (equal key (niwa-resource--memory-root-key root)))
         roots)
      (let ((matches
             (cl-remove-if-not
              (lambda (root) (equal key (plist-get root :alias)))
              roots)))
        (when (cdr matches)
          (signal 'niwa-resource-error
                  (list "Memory root alias is ambiguous")))
        (car matches)))))

(defun niwa-resource--canonical-relative (path root)
  "Return PATH relative to ROOT with canonical slash separators."
  (mapconcat #'identity
             (file-name-split (file-relative-name path root))
             "/"))

(defun niwa-resource--safe-path (root components)
  "Return a contained path below ROOT for decoded COMPONENTS.

The lexical parser rejects traversal.  This second check rejects symlink
escapes and is deliberately performed during preparation, before a handler
or permission callback sees a target."
  (when root
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (path (expand-file-name
                  (mapconcat #'identity components "/") root)))
      (unless (niwa-resource-within-root-p path root)
        (signal 'niwa-resource-error
                (list "Resource address escapes its owning root")))
      ;; Canonical before authorization: handlers open it through a pin that
      ;; refuses symlinks, so a legitimately linked root resolves here.
      (niwa-file-truename path))))

(defun niwa-resource--file-list (root &optional scheme)
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
       (delete-dups (delq nil
             (mapcar
              (lambda (path)
                (when (and (file-regular-p path)
                           (niwa-resource-within-root-p path root))
                  (let ((relative (niwa-resource--canonical-relative
                                   path root)))
                    (unless (or (and (eq scheme 'artifact)
                                     (niwa-execution-spool-private-p relative))
                                (and (eq scheme 'niwa)
                                     (not (string-suffix-p ".md" relative))))
                      (if (and (eq scheme 'artifact)
                               (member "executions" (file-name-split relative)))
                          (niwa-execution-spool-logical-name relative)
                        relative)))))
              (directory-files-recursively root "." nil nil nil))))
       #'string-lessp))))

(defun niwa-resource--logical-address (scheme components)
  "Return canonical SCHEME address for decoded COMPONENTS."
  (concat (symbol-name scheme) "://"
          (niwa-resource--canonical-components components)))

(defun niwa-resource--directory-list-result (scheme root)
  "Return a logical listing for directory-backed SCHEME ROOT."
  (let ((entries (niwa-resource--file-list root scheme)))
    (if entries
        (string-join
         (mapcar (lambda (entry)
                   (niwa-resource--logical-address
                    scheme (split-string entry "/" t)))
                 entries)
         "\n")
      (format "No files found under %s://" (symbol-name scheme)))))

(defun niwa-resource-parse-address (address)
  "Parse ADDRESS and return a locator plist.

ADDRESS may spell components with literal characters or escapes and may end
a directory with one slash; `:canonical' is the single spelling of its
identity.  The plist contains decoded `:components', canonical `:canonical', and
`:locator-class' (`exact', `alias', `session-relative', `workspace-relative',
or `dynamic').
Physical resolution is intentionally not performed here."
  (unless (stringp address)
    (signal 'niwa-resource-error (list "Resource address must be text")))
  (when (string-match-p "\\?" address)
    (signal 'niwa-resource-error
            (list "Resource addresses do not support query strings")))
  (let* ((name (niwa-resource--scheme-prefix address))
         (scheme (and name (niwa-resource-supported-scheme-p name))))
    (unless name
      (signal 'niwa-resource-error
              (list "Resource address must use a supported scheme")))
    (unless scheme
      (signal 'niwa-resource-error
              (list (format "Unsupported resource scheme: %s" name))))
    (let* ((prefix (concat name "://"))
           (tail (substring address (length prefix)))
           (fragment-data nil)
           (fragment-p (and (eq scheme 'agent)
                            (string-match "#" tail))))
      (when fragment-p
        (setq fragment-data
              (niwa-resource--decode-fragment (substring tail (1+ fragment-p))))
        (setq tail (substring tail 0 fragment-p)))
      ;; Directories are commonly written with a trailing slash; it names
      ;; nothing beyond the directory itself.
      (when (and (> (length tail) 1) (string-suffix-p "/" tail))
        (setq tail (substring tail 0 -1)))
      (when (and (not (eq scheme 'agent)) (string-match "#" tail))
        (signal 'niwa-resource-error
                (list "Fragments are not supported by this resource scheme")))
      (let* ((specific
              (pcase scheme
                ('skill (niwa-resource--parse-skill-tail tail))
                ('history
                 (let ((parts (niwa-resource--parse-components tail)))
                   (cond
                    ((equal parts '("root"))
                     (list :components parts :dynamic-p nil))
                    ((equal (car parts) "saved")
                     (unless (and (<= (length parts) 3)
                                  (or (< (length parts) 3)
                                      (niwa-session-artifacts-segment-number (caddr parts))))
                       (signal 'niwa-resource-error '("Saved history names a session and canonical segment")))
                     (list :components parts :dynamic-p (= (length parts) 1)
                           :locator-class (unless (= (length parts) 1)
                                            'workspace-relative)))
                    (t (niwa-resource--parse-agent-history-components parts)))))
                ('agent
                 (niwa-resource--parse-agent-history-components
                  (niwa-resource--parse-components tail)))
                ('memory (niwa-resource--parse-memory-tail tail))
                ('mcp (niwa-resource--parse-mcp-tail tail))
                ('notes
                 (let ((parts (niwa-resource--parse-components tail)))
                   (unless (or (null parts)
                               (and (= (length parts) 1)
                                    (string-match-p
                                     "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\.md\\'"
                                     (car parts))))
                     (signal 'niwa-resource-error
                             '("Notes use notes:// or notes://UUID.md from discovery")))
                   (list :components parts :dynamic-p (null parts)
                         :locator-class (and parts 'workspace-relative))))
                (_ (list :components (niwa-resource--parse-components tail)
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
                    (concat (niwa-resource-encode-component
                             (plist-get specific :name)) "@"
                            (plist-get specific :source-key)
                            (if components
                                (concat "/"
                                        (niwa-resource--canonical-components
                                         components))
                              "")))
                 (t "")))
               ((and (eq scheme 'memory) (equal components '("journal")))
                "journal/")
               (t (niwa-resource--canonical-components components))))
             (canonical (concat prefix canonical-tail
                                (if fragment-p
                                    (concat "#" (plist-get fragment-data :raw))
                                  "")))
             (class
              (or (plist-get specific :locator-class)
                  (cond
                   ((plist-get specific :dynamic-p) 'dynamic)
                   ((and (eq scheme 'work) (niwa-resource--shared-work-p components))
                    'exact)
                   ((memq scheme '(work artifact agent history shared))
                    'session-relative)
                   (t 'exact)))))
        (when (and (eq scheme 'agent)
                   fragment-p
                   (null components))
          (signal 'niwa-resource-error
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

(defun niwa-resource--root (scheme session)
  "Return the physical root for SCHEME and SESSION, without creating it."
  (if (eq scheme 'niwa)
      (file-name-concat niwa-resource--source-dir "docs")
    (let ((save-path (and session (niwa-session-save-path session))))
      (and save-path
           (pcase scheme
             ('work (file-name-concat save-path "local"))
             ('artifact (file-name-concat save-path "tool-results")))))))

(defconst niwa-resource--shared-work-component "shared"
  "First `work://' component that addresses the workspace's shared files.")

(defconst niwa-resource-shared-work-address
  (concat "work://" niwa-resource--shared-work-component)
  "Canonical address of the workspace's shared working files.")

(defun niwa-resource-work-shared-directory (workspace)
  "Return WORKSPACE's shared working directory without creating or probing it.
Resolution for access goes through `niwa-resource--work-location', which
also rejects symlink escapes."
  (when workspace
    (file-name-concat (niwa-workspace-root workspace)
                      ".niwa" niwa-resource--shared-work-component)))

(defun niwa-resource--shared-work-p (components)
  "Return non-nil when work COMPONENTS address the workspace's shared files."
  (equal (car components) niwa-resource--shared-work-component))

(defvar niwa-resource--shared-accesses (make-hash-table :test #'equal)
  "Session tool accesses to shared working files in this Emacs.
Maps an expanded workspace root directory to a table from relative path to
a weak-keyed table of SESSION to its latest access `float-time'.")

(defun niwa-resource--record-shared-access (workspace relative session)
  "Record SESSION's tool access to shared RELATIVE in WORKSPACE.
This is evidence of use for shared-note processing, not an authority."
  (when-let* ((root (and workspace session (niwa-workspace-root workspace))))
    (let* ((key (file-name-as-directory (expand-file-name root)))
           (paths (or (gethash key niwa-resource--shared-accesses)
                      (puthash key (make-hash-table :test #'equal)
                               niwa-resource--shared-accesses)))
           (sessions (or (gethash relative paths)
                         (puthash relative (make-hash-table :test #'eq :weakness 'key)
                                  paths))))
      (puthash session (float-time) sessions))))

(defun niwa-resource-shared-accesses (workspace)
  "Return WORKSPACE's recorded shared-file accesses.
Each element is (RELATIVE . ACCESSES), ACCESSES being (SESSION . TIME) rows."
  (when-let* ((root (and workspace (niwa-workspace-root workspace)))
              (paths (gethash (file-name-as-directory (expand-file-name root))
                              niwa-resource--shared-accesses)))
    (let (rows)
      (maphash (lambda (relative sessions)
                 (let (accesses)
                   (maphash (lambda (session time) (push (cons session time) accesses)) sessions)
                   (when accesses (push (cons relative accesses) rows))))
               paths)
      rows)))

(defun niwa-resource-session-work-p (address)
  "Return non-nil if ADDRESS names a session-owned work descendant."
  (when (and (stringp address) (string-prefix-p "work://" address))
    (condition-case nil
        (let ((parts (plist-get (niwa-resource-parse-address address) :components)))
          (and parts (not (niwa-resource--shared-work-p parts))))
      (niwa-resource-error nil))))

(defun niwa-resource--work-location (components context session)
  "Return (ROOT . RELATIVE) for work COMPONENTS in CONTEXT and SESSION.
Shared components resolve below the workspace's contained shared directory,
other components below the session's own work root.  ROOT is nil when the
owner is unavailable."
  (if (niwa-resource--shared-work-p components)
      (cons (when-let* ((workspace (niwa-resource--workspace context session)))
              (niwa-resource--safe-path (niwa-workspace-root workspace)
                                          (list ".niwa" niwa-resource--shared-work-component)))
            (cdr components))
    (cons (niwa-resource--root 'work session) components)))

(defun niwa-resource--niwa-path-available-p
    (root path components operation)
  "Return non-nil when PATH is an exposed Niwa document scope.

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

(defconst niwa-resource--shared-identity "\\`[a-zA-Z0-9_-]\\{1,80\\}\\'"
  "Identity of a shared item, element, block or image.")

(defun niwa-resource--shared-shape-p (components)
  "Return non-nil when COMPONENTS name a `shared://' resource.
Items are `ID' with `view.png', `discussions', `history', `elements/ID' and
`images/KEY' descendants; a remote HTML artifact has `files', listing its
files, and `files/PATH', one file.  `library' lists element libraries,
optionally one library by name, each with a `sheet.png', and previews one
item at `LIBRARY/ITEM-ID/view.png'.  A remote whiteboard's `ID/library'
has the same shapes for its origin's libraries."
  (cl-flet ((id-p (value)
              (and (stringp value)
                   (string-match-p niwa-resource--shared-identity value)))
            (library-p (rest)
              (pcase rest
                ((or 'nil `(,_) `(,_ "sheet.png") `(,_ ,_ "view.png")) t))))
    (pcase components
      ('nil t)
      (`("library" . ,rest) (library-p rest))
      (`(,id) (id-p id))
      (`(,id ,(or "view.png" "discussions" "history")) (id-p id))
      (`(,id "library" . ,rest) (and (id-p id) (library-p rest)))
      (`(,id ,(or "elements" "images") ,name) (and (id-p id) (id-p name)))
      (`(,id "files" . ,path)
       (and (id-p id)
            (cl-every (lambda (part) (not (member part '("" "." ".."))))
                      path))))))

(defun niwa-resource-shared-file-p (components)
  "Return non-nil when `shared://' COMPONENTS name a remote artifact's file.
Such an address, shared://ID/files/PATH, is an HTML artifact's file at
its origin: content, never a local path."
  (and (equal (cadr components) "files") (cddr components) t))

(defun niwa-resource-remote-file (address)
  "Return (ID . FILE) when ADDRESS is shared://ID/files/FILE, else nil.
FILE is the file's path relative to the remote artifact's directory."
  (when (and (stringp address) (string-prefix-p "shared://" address))
    (condition-case nil
        (let ((components (plist-get (niwa-resource-parse-address address) :components)))
          (when (and (niwa-resource-shared-file-p components)
                     (niwa-resource--shared-shape-p components))
            (cons (car components) (string-join (cddr components) "/"))))
      (niwa-resource-error nil))))

(defun niwa-resource--shared-available-p (components session)
  "Return non-nil when shared COMPONENTS resolve in SESSION's workspace.
Item addresses need an existing item, and `files' and `ID/library'
addresses a remote one; listings and libraries always resolve."
  (or (null components)
      (equal (car components) "library")
      (and session
           (if (member (cadr components) '("files" "library"))
               (niwa-artifact-backend-registered-p (niwa-session-workspace session)
                                                      (car components))
             (niwa-artifact-backend-present-p (niwa-session-workspace session)
                                                 (car components)))
           t)))

(defun niwa-resource--shared-list-result (session)
  "Return the workspace's shared item listing for a bare `shared://' Read.
Items attached to SESSION are marked."
  (let ((items (and session (niwa-artifact-backend-catalog
                             (niwa-session-workspace session) 'items)))
        (attached (and session (niwa-session-attached-artifacts session))))
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

(defun niwa-resource--mcp-servers ()
  "Return current MCP server metadata, or signal when mcp.el is absent."
  (unless (fboundp 'mcp-hub-get-servers)
    (signal 'niwa-resource-unavailable
            (list "MCP support is unavailable")))
  (or (mcp-hub-get-servers) nil))

(defun niwa-resource--mcp-server (name)
  "Return current MCP metadata for server NAME, or nil."
  (cl-find-if (lambda (server)
                (equal (plist-get server :name) name))
              (niwa-resource--mcp-servers)))

(defun niwa-resource--mcp-connection (name)
  "Return the current MCP connection for NAME, or nil."
  (and (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (gethash name mcp-server-connections)))

(defun niwa-resource--mcp-address (server uri)
  "Return the canonical MCP address for SERVER and native URI URI."
  (concat "mcp://"
          (niwa-resource-encode-component server)
          "/"
          (niwa-resource-encode-component uri)))

(defun niwa-resource-mcp-extract-text (response)
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

(defun niwa-resource--mcp-list-result (&optional server-info)
  "Return a model-visible listing for SERVER-INFO or all MCP servers."
  (if (null server-info)
      (let ((servers (sort (copy-sequence (niwa-resource--mcp-servers))
                           (lambda (left right)
                             (string-lessp (or (plist-get left :name) "")
                                           (or (plist-get right :name) ""))))))
        (if servers
            (string-join
             (mapcar (lambda (server)
                       (format "mcp://%s\t%s"
                               (niwa-resource-encode-component
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
          (signal 'niwa-resource-unavailable
                  (list (format "MCP server %s is not connected" server)))
        (if resources
            (string-join
             (mapcar (lambda (resource)
                       (format "%s\t%s"
                               (niwa-resource--mcp-address
                                server (plist-get resource :uri))
                               (or (plist-get resource :name)
                                   (plist-get resource :description)
                                   "")))
                     resources)
             "\n")
          (format "No MCP resources advertised by %s" server))))))

(defun niwa-resource--mcp-read (server uri)
  "Read URI from connected MCP SERVER using the mention contract."
  (let ((server-info (niwa-resource--mcp-server server)))
    (unless server-info
      (signal 'niwa-resource-unavailable
              (list (format "Unknown MCP server: %s" server))))
    (unless (eq 'connected (plist-get server-info :status))
      (signal 'niwa-resource-unavailable
              (list (format "MCP server %s is not connected" server))))
    (let ((connection (niwa-resource--mcp-connection server)))
      (unless connection
        (signal 'niwa-resource-unavailable
                (list (format "No active MCP connection to %s" server))))
      (condition-case err
          (let* ((response (mcp-read-resource connection uri))
                 (text (niwa-resource-mcp-extract-text response)))
            (if (string-empty-p text)
                (format "%s (%s)"
                        (if (seq-empty-p (plist-get response :contents))
                            "MCP resource returned no content"
                          "MCP resource returned no text; this Read surface exposes text content only")
                        (niwa-resource--mcp-address server uri))
              text))
        (error
         (signal 'niwa-resource-unavailable
                 (list (format "MCP resource read failed: %s"
                               (error-message-string err)))))))))

(defun niwa-resource--agent-record (path session)
  "Return retained agent record for canonical PATH in SESSION."
  (when session
    (cdr (assoc path (niwa-session-agent-registry session)))))

(defun niwa-resource--agent-list-result (session &optional history-p)
  "Return a sorted agent resource listing for SESSION.
When HISTORY-P is non-nil, include root and retained conversation histories."
  (let ((entries
         (cl-loop for entry in
                  (plist-get
                   (niwa-resource-completion-metadata
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
                                         (niwa-agent-control-settled-result
                                          record)))))
                    (format "%s\t%s\t%s"
                            address
                            (or (plist-get item :role) "default")
                            (if ready "ready" "not-ready"))))))
    (if entries
        (string-join (sort entries #'string-lessp) "\n")
      (if history-p "No conversation histories"
        "No retained agents"))))

(defconst niwa-resource--json-null
  (make-symbol "niwa-resource-json-null")
  "Sentinel for a parsed JSON null value.")

(defconst niwa-resource--json-false
  (make-symbol "niwa-resource-json-false")
  "Sentinel for a parsed JSON false value.")

(defconst niwa-resource--json-missing
  (make-symbol "niwa-resource-json-missing")
  "Sentinel for a missing JSON Pointer value.")

(defun niwa-resource--json-parse (payload)
  "Parse complete JSON PAYLOAD with sentinels for null, false, and missing."
  (condition-case err
      (json-parse-string
       payload :object-type 'alist :array-type 'array
       :null-object niwa-resource--json-null
       :false-object niwa-resource--json-false)
    (error
     (signal 'niwa-resource-unavailable
             (list (format "Agent result is not valid JSON; Read without a JSON pointer to inspect the full result: %s"
                           (error-message-string err)))))))

(defun niwa-resource--json-pointer-value (value tokens)
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
                (if entry (cdr entry) niwa-resource--json-missing)))
             ((vectorp current)
              (if (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" token)
                  (let ((index (string-to-number token)))
                    (if (< index (length current))
                        (aref current index)
                      niwa-resource--json-missing))
                niwa-resource--json-missing))
             (t niwa-resource--json-missing)))
      (when (eq current niwa-resource--json-missing)
        (signal 'niwa-resource-unavailable
                (list (format "JSON Pointer component is missing: %s" token)))))))

(defun niwa-resource--json-render (value &optional nested)
  "Render parsed JSON VALUE as readable scalar or deterministic JSON.
When NESTED in an array or object, retain JSON string quoting."
  (cond
   ((eq value niwa-resource--json-null) "null")
   ((eq value niwa-resource--json-false) "false")
   ((eq value t) "true")
   ((stringp value)
    (if nested (json-serialize value) value))
   ((numberp value) (number-to-string value))
   ((vectorp value)
    (concat "["
            (string-join
             (mapcar (lambda (item) (niwa-resource--json-render item t))
                     (append value nil)) ",")
             "]"))
   ((listp value)
    (concat "{"
            (string-join
             (mapcar (lambda (entry)
                       (format "%s:%s"
                               (json-serialize (symbol-name (car entry)))
                               (niwa-resource--json-render (cdr entry) t)))
                     (sort (copy-sequence value)
                           (lambda (left right)
                             (string-lessp (car left) (car right)))))
             ",")
            "}"))
   (t (json-serialize (format "%s" value)))))

(defun niwa-resource--agent-read (record parsed)
  "Return settled RECORD payload or active-turn status for PARSED address."
  (let ((settled (and record
                      (niwa-agent-control-settled-result record))))
    (cond
     ((and record
           (niwa-agent-control-active-activity-p
            (niwa-agent-record-activity record)))
      (let ((path (niwa-resource--canonical-components
                   (plist-get parsed :components)))
            (history-p (or (buffer-live-p
                            (niwa-agent-record-conversation-buffer record))
                           (niwa-agent-record-conversation-location record))))
        (list :resource-status 'not-ready
              :result
              (format "NOT READY: agent://%s has an active turn (%s); wait for completion.\n%s"
                      path (niwa-agent-record-activity record)
                      (if history-p
                          (format "Inspect progress with Read(file_path=\"history://%s\")." path)
                        "Conversation history is not yet available.")))))
     ((not settled)
      (signal 'niwa-resource-unavailable
              '("Retained agent has no settled result; its idle record is unavailable")))
     (t
      (let ((payload (plist-get settled :payload)))
        (if (not (plist-get parsed :fragment-p))
            payload
          (let* ((value (niwa-resource--json-parse payload))
                 (selected (niwa-resource--json-pointer-value
                            value (plist-get parsed :pointer))))
            (niwa-resource--json-render selected))))))))

(defun niwa-resource--history-hydrate (record session)
  "Load only RECORD's conversation from SESSION for history inspection.
Use the live root's context when available; otherwise use a temporary read-only
context.  Other retained agents and their activity remain untouched."
  (let* ((live (niwa-session-root-buffer session))
         (temporary (not (buffer-live-p live)))
         (root-buffer (if temporary
                          (generate-new-buffer " *niwa-resource-history-root*")
                        live)))
    (unwind-protect
        (progn
          (when temporary
            (with-current-buffer root-buffer
              (niwa--transcript-org-mode)
              (setq-local niwa--session session)
              (setq-local niwa--workspace (niwa-session-workspace session))))
          (niwa-agent-persistence-ensure-conversation
           session record root-buffer
           (or temporary (buffer-local-value 'niwa-session--read-only-mode root-buffer))))
      (when (and temporary (buffer-live-p root-buffer))
        (kill-buffer root-buffer)))))

(defun niwa-resource--history-read (record session)
  "Return concise Markdown for RECORD, or SESSION's root when RECORD is nil."
  (let ((buffer (if record
                    (niwa-agent-record-conversation-buffer record)
                  (and session (niwa-session-root-buffer session)))))
    (when (and record (not (buffer-live-p buffer)))
      (setq buffer (niwa-resource--history-hydrate record session)))
    (unless (buffer-live-p buffer)
      (signal 'niwa-resource-unavailable
              (list "Root conversation is unavailable")))
    (niwa-agent-conversation-project-history buffer session)))

(defun niwa-resource--skill-list-result (session context)
  "Return a canonical listing of discoverable skills."
  (let ((skills
         (cl-remove-if-not
          (lambda (skill)
            (or (not (fboundp 'niwa-skills-skill-enabled-p))
                (niwa-skills-skill-enabled-p skill)))
          (niwa-resource--skill-list session context))))
    (if skills
        (let ((alias-counts (make-hash-table :test #'equal))
              rows)
          (dolist (skill skills)
            (when-let* ((alias (niwa-resource--skill-alias-address skill)))
              (puthash alias (1+ (gethash alias alias-counts 0)) alias-counts)))
          (dolist (skill skills)
            (let ((description (or (niwa-skill-description skill) ""))
                  (alias (niwa-resource--skill-alias-address skill)))
              (push (cons (niwa-resource--skill-address skill) description)
                    rows)
              (when (and alias (= 1 (gethash alias alias-counts)))
                (push (cons alias description) rows))))
          (string-join
           (mapcar (lambda (row) (format "%s\t%s" (car row) (cdr row)))
                   (sort rows (lambda (left right)
                                (string-lessp (car left) (car right)))))
           "\n"))
      "No discoverable skills")))

(defun niwa-resource--memory-search-roots (context session)
  "Return exact helper roots for the configured memory union.

Configured roots whose directory does not exist are excluded, matching
the union index read, which already tolerates missing roots."
  (let ((roots (niwa-resource--memory-roots context session)))
    (mapcar
     (lambda (root)
       (list :path (plist-get root :dir)
             :address-prefix
             (concat "memory://"
                     (niwa-resource--memory-root-address-key root roots))
             :label (plist-get root :label)))
     (cl-remove-if-not
      (lambda (root) (file-directory-p (plist-get root :dir)))
      roots))))

(defun niwa-resource--execute-logical (data options)
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
       (let* ((shared (car (niwa-resource--work-location
                           (list niwa-resource--shared-work-component) context session)))
              (shared-p (equal components '("shared")))
              (empty (if shared-p
                         "No shared working files yet. Create a file under work://shared/ using ApplyPatch when workspace writes are permitted."
                       "No working files found under work://."))
              (roots (cl-remove-if-not
                      (lambda (entry)
                        (and (plist-get entry :path)
                             (file-directory-p (plist-get entry :path))))
                      (list (list :path (unless shared-p root) :address "work://")
                            (list :path shared :address-prefix niwa-resource-shared-work-address)))))
         (if (eq operation 'read)
             (let ((files
                    (cl-loop for entry in roots
                             append
                             (if (plist-get entry :address-prefix)
                                 (list (niwa-working-note-files-listing
                                        (plist-get entry :path) niwa-resource-shared-work-address))
                               (mapcar
                                (lambda (file)
                                  (niwa-resource--logical-address 'work (split-string file "/" t)))
                                (niwa-resource--file-list (plist-get entry :path)))))))
               (if files (string-join files "\n") empty))
           (list :resource-search-roots roots :empty-result empty))))
      ('artifact
       (if (null components)
           (if (eq operation 'read)
               (niwa-resource--directory-list-result 'artifact root)
             (list :resource-search-roots
                   (when (and root (file-directory-p root))
                     (list (list :path root :address "artifact://")))
                   :empty-result "No persisted tool results found under artifact://."))
         (signal 'niwa-resource-unavailable
                 (list "Internal resource error: artifact file reached discovery execution"))))
      ('niwa
       (if (null components)
           (niwa-resource--directory-list-result 'niwa root)
         (signal 'niwa-resource-unavailable
                 (list "Internal resource error: documentation file reached discovery execution"))))
      ('skill
       (if (null (plist-get data :source-file))
           (niwa-resource--skill-list-result session context)
         (signal 'niwa-resource-unavailable
                 (list "Internal resource error: skill file reached discovery execution"))))
      ('agent
       (if (null components)
           (niwa-resource--agent-list-result session)
         (niwa-resource--agent-read
          (plist-get data :record) parsed)))
      ('notes (niwa-note-resource-execute operation components session))
      ('history
       (cond
        ((equal (car components) "saved")
         (list :history-workspace (niwa-resource--workspace context session)
               :history-components components))
        ((null components)
         (concat (niwa-resource--agent-list-result session t)
                 (when (niwa-resource--workspace context session)
                   "\nhistory://saved\tSaved workspace conversations (Read, Glob, Grep)")))
        (t
         (let ((text (niwa-resource--history-read
                      (plist-get data :record) session)))
           (if (eq operation 'grep)
               (list :resource-search-documents
                     (list (cons (car (last components)) text)))
             text)))))
      ((and 'memory (guard (equal (car components) "journal")))
       (let* ((components (cdr components))
              (workspace (niwa-resource--workspace context session))
              (workspace-root (and workspace (niwa-workspace-root workspace))))
         (unless workspace-root
           (signal 'niwa-resource-unavailable '("Journal requires a workspace")))
         (condition-case err
             (let ((entries (if components
                                (list (niwa-journal-store-read
                                       workspace-root (car components)))
                              (niwa-journal-store-entries workspace-root))))
               (setq entries (seq-filter #'niwa-journal-store-recall-p entries))
               (when (and components (null entries))
                 (signal 'niwa-journal-store-invalid
                         '("Journal entry has expired from ordinary recall")))
               (if (eq operation 'read)
                   (if components
                       (plist-get (car entries) :text)
                     (if entries
                         (mapconcat
                          (lambda (entry)
                            (concat "memory://journal/"
                                    (niwa-resource-encode-component
                                     (plist-get entry :file))))
                          entries "\n")
                       "No published journal entries"))
                 (list :resource-search-documents
                       (mapcar (lambda (entry)
                                 (cons (plist-get entry :file) (plist-get entry :text)))
                               entries))))
           ((file-missing niwa-session-control-fs-absent)
            (signal 'niwa-resource-unavailable
                    '("Journal entry not found; Read memory://journal/ to discover published entries")))
           (niwa-journal-store-invalid
            (signal 'niwa-resource-unavailable
                    (list (format "Invalid journal entry: %s"
                                  (car (last (cdr err)))))))
           (file-error
            (signal 'niwa-resource-unavailable
                    '("Journal storage could not be read; check workspace storage access")))
           (error
            (signal 'niwa-resource-unavailable
                    (list (format "Journal read failed: %s"
                                  (niwa-resource-error-message
                                   err (plist-get data :address) (list root)))))))))
      ('memory
       (if (equal components '("root"))
           (pcase operation
             ('read
              (if (niwa-resource--memory-roots context session)
                  (niwa-system--memory-content
                   (niwa-resource--workspace context session))
                "No memory roots configured."))
             ((or 'glob 'grep)
              (list :resource-search-roots
                    (niwa-resource--memory-search-roots
                     context session)
                    :empty-result
                    (if (niwa-resource--memory-roots context session)
                        "No memory files found under memory://root."
                      "No memory roots configured."))))
         (signal 'niwa-resource-unavailable
                 (list "Internal resource error: memory file reached discovery execution"))))
      ('shared
       (if (and (null components) (eq operation 'read))
           (niwa-resource--shared-list-result session)
         ;; Everything else is computed by the workspace's editing host, which
         ;; answers asynchronously; Read and Grep fetch it themselves.
         (list :shared-view components)))
      ('mcp
       (cond
        ((null components)
         (niwa-resource--mcp-list-result))
        ((null (cdr components))
         (let ((server-info (niwa-resource--mcp-server (car components))))
           (unless server-info
             (signal 'niwa-resource-unavailable
                     (list (format "Unknown MCP server: %s"
                                   (car components)))))
           (niwa-resource--mcp-list-result server-info)))
        (t (niwa-resource--mcp-read
            (car components) (cadr components)))))
      (_ (signal 'niwa-resource-error
                 (list "Internal resource error: unknown provider"))))))

(defun niwa-resource-within-root-p (path root)
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

(defun niwa-resource--skill-physical-path (skill root components)
  "Return the contained source or package path for SKILL.

COMPONENTS are relative to the selected skill package.  The source file is
checked through the same containment seam as package descendants rather
than trusting the discovery record's pathname."
  (if components
      (niwa-resource--safe-path root components)
    (niwa-resource--safe-path
     root
     (split-string (file-relative-name
                    (niwa-skill-source-file skill) root)
                   "/" t))))

(defun niwa-resource--refresh-data (data)
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
      (pcase-let ((`(,owner . ,relative) (niwa-resource--work-location components context session)))
        (setq root owner
              physical (niwa-resource--safe-path owner relative))))
     ((eq scheme 'artifact)
      (setq root (niwa-resource--root scheme session)
            physical (niwa-resource--safe-path root components))
      (when (or (niwa-execution-spool-private-p (string-join components "/"))
                (and (equal (car components) "executions")
                     (string-suffix-p ".log.gz" (car (last components)))))
        (setq data (plist-put data :unavailable-p t))))
     ((eq scheme 'niwa)
      (setq root (niwa-resource--root scheme nil)
            physical (niwa-resource--safe-path root components))
      (unless (niwa-resource--niwa-path-available-p
               root physical components operation)
        (setq data (plist-put data :unavailable-p t))))
     ((eq scheme 'skill)
      (unless (plist-get data :dynamic-p)
        (let ((skill (niwa-resource--skill-for-digest
                      (plist-get data :source-key) session context)))
          (if (not skill)
              (setq data (plist-put data :unavailable-p t))
            (let ((source-file (niwa-skill-source-file skill)))
              (setq root (niwa-resource--skill-root skill)
                    data (plist-put data :source-file source-file)
                    physical
                    (if (memq operation '(glob grep))
                        (niwa-resource--safe-path root components)
                      (niwa-resource--skill-physical-path
                       skill root components)))
              (unless (and (file-regular-p source-file)
                           (file-directory-p root)
                           (if (eq operation 'read)
                               (file-regular-p physical)
                             (file-exists-p physical)))
                (setq data (plist-put data :unavailable-p t))))))))
     ((and (eq scheme 'history) (equal (car components) "saved"))
      (unless (niwa-resource--workspace context session)
        (setq data (plist-put data :unavailable-p t))))
     ((and (eq scheme 'history) (equal components '("root")))
      (unless (and session
                   (buffer-live-p (niwa-session-root-buffer session)))
        (setq data (plist-put data :unavailable-p t))))
     ((memq scheme '(agent history))
      (when components
        (let ((record
               (and (equal (car components) "root")
                    (niwa-resource--agent-record
                     (concat "/" (string-join components "/")) session))))
          (setq data (plist-put data :record record))
          (unless record
            (setq data (plist-put data :unavailable-p t))))))
     ((and (eq scheme 'memory) (equal (car components) "journal"))
      (if-let* ((workspace (niwa-resource--workspace context session)))
          (progn
            (setq root (niwa-journal-store-directory (niwa-workspace-root workspace)))
            (niwa-resource--safe-path root (cdr components)))
        (setq data (plist-put data :unavailable-p t))))
     ((eq scheme 'memory)
      (unless (equal components '("root"))
        (let ((memory-root
               (niwa-resource--memory-root-for-key
                (car components) context session)))
          (if (not memory-root)
              (setq data (plist-put data :unavailable-p t))
            (setq root (plist-get memory-root :dir)
                  physical (niwa-resource--safe-path
                            root (cdr components))
                  data (plist-put data :memory-root memory-root))))))
     ((eq scheme 'shared)
      (unless (niwa-resource--shared-available-p components session)
        (setq data (plist-put data :unavailable-p t)))))
    (when (and (eq operation 'apply-patch)
               (or (eq scheme 'memory)
                   (and (eq scheme 'work) (niwa-resource--shared-work-p components)))
               (not (equal root (plist-get data :root))))
      (signal 'niwa-resource-error
              (list (format "Writable resource root changed after preparation: %s; retry the patch against the current workspace"
                            (plist-get data :address)))))
    (setq data (plist-put data :root root))
    (plist-put data :physical-path physical)))

(defun niwa-resource-prepare (operation address context)
  "Prepare ADDRESS for OPERATION in CONTEXT without reading its content.

The returned value is opaque to callers.  Ordinary filesystem paths return
nil; malformed addresses and unsupported operation pairs signal validation
errors before any content or handler is reached."
  (when (and (stringp address)
             (niwa-resource-address-like-p address))
    (condition-case err
        (let* ((parsed (niwa-resource-parse-address address))
           (scheme (plist-get parsed :scheme))
           (components (plist-get parsed :components))
           (session (niwa-resource--session context))
           (workspace (niwa-resource--workspace context session))
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
                       (or (memq scheme '(work artifact skill memory niwa notes))
                           (and (eq scheme 'history)
                                (or (equal (car components) "saved")
                                    (and (eq operation 'grep)
                                         (equal (car components) "root"))))))
                  (and (eq operation 'apply-patch)
                       (or (memq scheme '(work memory))
                           (and (eq scheme 'shared)
                                (niwa-resource-shared-file-p components)))))
        (signal 'niwa-resource-error
                (list (format "%s does not support %s:// resources"
                              (if (eq operation 'apply-patch) "ApplyPatch"
                                (capitalize (symbol-name operation))) scheme))))
      (when (and (eq operation 'apply-patch) (eq scheme 'memory)
                 (equal (car components) "journal"))
        (signal 'niwa-resource-error '("Journal evidence is read-only")))
      (when (and (eq scheme 'skill)
                 (plist-get parsed :dynamic-p)
                 (not (eq operation 'read)))
        (signal 'niwa-resource-error
                (list "Bare skill:// supports Read only; Read skill:// to discover resource addresses, or use ListSkills for model-invocable skills, then search a selected package")))
      ;; A bare directory address names a listing, never a patch endpoint.
      (when (and (null components)
                 (eq operation 'apply-patch))
        (signal 'niwa-resource-error
                (list (format "Bare %s:// is not a patch target"
                              (symbol-name scheme)))))
      (when (and (eq operation 'apply-patch)
                 (or (and (eq scheme 'memory) (< (length components) 2))
                     (and (eq scheme 'work) (niwa-resource--shared-work-p components)
                          (null (cdr components)))))
        (signal 'niwa-resource-error '("Patch targets must name a file descendant")))
      (cond
       ((eq scheme 'work)
        (pcase-let ((`(,owner . ,relative) (niwa-resource--work-location components context session)))
          (setq root owner
                physical (niwa-resource--safe-path owner relative)
                logical-p (or (null components)
                              (equal components '("shared")))))
        ;; A named shared file touched by a session's tool is in use; folder
        ;; listings and searches are not.  Sub-agents carry their parent session.
        (when (and (niwa-resource--shared-work-p components) (cdr components) physical
                   (not (file-directory-p physical)))
          (niwa-resource--record-shared-access
           workspace (string-join (cdr components) "/") session)))
       ((eq scheme 'artifact)
        (setq root (niwa-resource--root scheme session)
              physical (and root
                            (niwa-resource--safe-path root components))
              logical-p (null components))
        (when (or (niwa-execution-spool-private-p (string-join components "/"))
                  (and (equal (car components) "executions")
                       (string-suffix-p ".log.gz" (car (last components)))))
          (setq data (plist-put data :unavailable-p t))))
       ((eq scheme 'niwa)
        (setq root (niwa-resource--root scheme nil)
              physical (and root
                            (niwa-resource--safe-path root components))
              logical-p (and (null components)
                             (eq operation 'read)))
        (unless (niwa-resource--niwa-path-available-p
                 root physical components operation)
          (setq data (plist-put data :unavailable-p t))))
       ((eq scheme 'skill)
        (if (plist-get parsed :dynamic-p)
            (setq logical-p t)
          (let* ((alias-p (eq (plist-get parsed :locator-class) 'alias))
                 (skill
                  (if alias-p
                      (niwa-resource--skill-for-alias
                       parsed session context)
                    (niwa-resource--skill-for-digest
                     (plist-get parsed :source-key) session context))))
            (if (not skill)
                (setq data (plist-put data :unavailable-p t))
              (let* ((source-file (niwa-skill-source-file skill))
                     (source-key (niwa-resource-skill-digest source-file))
                     (exact-address
                      (niwa-resource--skill-address skill components)))
                (setq root (niwa-resource--skill-root skill)
                    data (plist-put data :source-key source-key)
                    data (plist-put
                          data :source-file
                          source-file)
                    data (plist-put
                          data :exact-address exact-address))
                (if (memq operation '(glob grep))
                    (setq physical
                          (niwa-resource--safe-path root components)
                          logical-p nil)
                  (setq physical
                        (niwa-resource--skill-physical-path
                         skill root components))))))))
       ((memq scheme '(agent history))
        (setq logical-p t)
        (cond
         ((and (eq scheme 'history) (equal (car components) "saved"))
          (unless workspace (setq data (plist-put data :unavailable-p t))))
         ((null components))
         ((and (eq scheme 'history) (equal components '("root")))
          (unless (and session
                       (buffer-live-p (niwa-session-root-buffer session)))
            (setq data (plist-put data :unavailable-p t))))
         (t
          (let ((record
                 (and (equal (car components) "root")
                      (niwa-resource--agent-record
                       (concat "/" (string-join components "/")) session))))
            (setq data (plist-put data :record record))
            (unless record
              (setq data (plist-put data :unavailable-p t)))))))
       ((and (eq scheme 'memory) (equal (car components) "journal"))
        (setq logical-p t)
        (when workspace
          (setq root (niwa-journal-store-directory (niwa-workspace-root workspace)))
          (niwa-resource--safe-path root (cdr components))))
       ((eq scheme 'memory)
        (if (equal components '("root"))
            (setq logical-p t)
          (let ((memory-root
                 (niwa-resource--memory-root-for-key
                  (car components) context session)))
            (if (not memory-root)
                (setq data (plist-put data :unavailable-p t))
              (setq root (plist-get memory-root :dir)
                    physical (niwa-resource--safe-path
                              root (cdr components))
                    data (plist-put data :memory-root memory-root))))))
       ((eq scheme 'notes)
        (setq logical-p t))
       ((eq scheme 'mcp)
        (setq logical-p t))
       ((eq scheme 'shared)
        (unless (niwa-resource--shared-shape-p components)
          (signal 'niwa-resource-error
                  (list "Unknown shared:// address; Read shared:// for this project's items")))
        (setq logical-p t)
        (unless (niwa-resource--shared-available-p components session)
          (setq data (plist-put data :unavailable-p t)))))
      (setq data (plist-put data :root root))
      (setq data (plist-put data :physical-path physical))
      (setq data (plist-put data :logical-p logical-p))
      (let ((attempt (make-symbol "niwa-resource-attempt-")))
        (puthash attempt data niwa-resource--attempt-table)
        (when-let* ((cell (or (plist-get context :resource-attempts-cell)
                             niwa-resource-attempts-cell)))
          (when (consp cell)
            (setcar cell (cons attempt (car cell)))))
        attempt))
      (niwa-resource-error
       (signal 'niwa-resource-error
               (list (niwa-resource-error-message err address)))))))

(defun niwa-resource--unavailable-reason (data)
  "Return the actionable availability failure for refreshed DATA, or nil."
  (let ((scheme (plist-get data :scheme))
        (session (plist-get data :session))
        (components (plist-get data :components))
        (root (plist-get data :root))
        (path (plist-get data :physical-path))
        (unavailable (plist-get data :unavailable-p)))
    (cond
     ((eq scheme 'notes)
      (unless (and session (niwa-session-workspace session))
        "Note resources require a session workspace"))
     ((and (eq scheme 'work) (niwa-resource--shared-work-p components))
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
     ((eq scheme 'niwa)
      (cond
       ((or (null root) (not (file-directory-p root))
            (file-symlink-p (directory-file-name root)))
        "Installed documentation is missing or its root is invalid")
       ((not (file-exists-p path)) "File not found")
       ((file-directory-p path)
        "Read requires a Markdown document; use Glob or Grep for documentation directories")
       (t "Only packaged Markdown documents are readable; Read niwa:// to discover documents")))
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

(defun niwa-resource-attempt-address (attempt)
  "Return ATTEMPT's authored address."
  (plist-get (gethash attempt niwa-resource--attempt-table) :address))

(defun niwa-resource-attempt-write-check (attempt)
  "Return a freshness check for ATTEMPT's workspace or memory write target.
The closure survives attempt consumption and is checked through patch review
and commit.  It does not grant authority or read file content.  A remote
artifact's file stays current while the artifact remains attached."
  (let ((data (copy-sequence (gethash attempt niwa-resource--attempt-table))))
    (cond
     ((not (eq (plist-get data :operation) 'apply-patch)) nil)
     ((eq (plist-get data :scheme) 'shared)
      (let ((workspace (plist-get data :workspace))
            (id (car (plist-get data :components))))
        (lambda ()
          (unless (and workspace (niwa-artifact-backend-registered-p workspace id))
            (signal 'niwa-resource-error
                    (list (format "Remote artifact %s was detached during review: %s"
                                  id (plist-get data :address)))))
          t)))
     ((not (niwa-resource-session-work-p (plist-get data :address)))
      (let* ((root (plist-get data :root))
             (remote-file-name-inhibit-cache t)
             (identity (and root (file-truename root))))
        (lambda ()
          (let* ((remote-file-name-inhibit-cache t)
                 (fresh (niwa-resource--refresh-data (copy-sequence data))))
            (unless (and root
                         (not (plist-get fresh :unavailable-p))
                         (equal identity (file-truename root)))
              (signal 'niwa-resource-error
                      (list (format "Writable resource target changed during review: %s; review a fresh patch"
                                    (plist-get data :address)))))
            t)))))))

(defun niwa-resource-attempt-write-path (attempt)
  "Return ATTEMPT's prepared backing path for filesystem write policy.
Session-owned scratch keeps its address-based permission treatment.  A
remote artifact's file has no backing path: it is content at its origin,
authorized as an edit like SharedEdit's, not as a local file."
  (let ((data (gethash attempt niwa-resource--attempt-table)))
    (when (and (eq (plist-get data :operation) 'apply-patch)
               (not (eq (plist-get data :scheme) 'shared))
               (not (niwa-resource-session-work-p (plist-get data :address))))
      (or (plist-get data :physical-path)
          (signal 'niwa-resource-unavailable
                  (list (niwa-resource-error-message
                         (list 'niwa-resource-unavailable
                               (or (niwa-resource--unavailable-reason
                                    (niwa-resource--refresh-data (copy-sequence data)))
                                   "Writable resource owner is unavailable"))
                         (plist-get data :address))))))))

(defun niwa-resource-execute (attempt &optional executor options)
  "Execute authorized opaque ATTEMPT.

For file-backed resources, EXECUTOR receives the private physical path and
authored address, preserving the initial filesystem-owner seam.  For virtual
resources, EXECUTOR receives a result descriptor and authored address; the
descriptor has `:virtual', `:result', `:address', and `:scheme'.  Without an
EXECUTOR, virtual resources return that descriptor.  OPTIONS replaces the
prepared operation options for virtual execution and is never reparsed as an
address.  No backing path is returned for a file-backed attempt without an
executor."
  (let ((data (gethash attempt niwa-resource--attempt-table)))
    (unwind-protect
        (condition-case err
            (progn
              (unless (plist-get data :resource-p)
                (signal 'niwa-resource-error
                        (list "Internal resource error: invalid execution attempt")))
              (setq data (niwa-resource--refresh-data data))
              (when-let* ((reason (niwa-resource--unavailable-reason data)))
                (signal 'niwa-resource-unavailable
                        (list reason)))
              (when (and (plist-get data :logical-p)
                         (memq (plist-get data :scheme) '(work artifact))
                         (plist-get data :root)
                         (file-exists-p (plist-get data :root))
                         (not (file-directory-p (plist-get data :root))))
                (signal 'niwa-resource-unavailable
                        '("Resource storage root is not a directory")))
              (if (plist-get data :logical-p)
                  (let* ((result (niwa-resource--execute-logical data options))
                         (descriptor (list :virtual t
                                           :result (if (and (listp result)
                                                            (plist-member result :resource-search-roots)
                                                            (null (plist-get result :resource-search-roots)))
                                                       (plist-get result :empty-result)
                                                     result)
                                           :address (plist-get data :address)
                                           :scheme (plist-get data :scheme)
                                           :operation (plist-get data :operation))))
                    (when (and (listp result) (plist-member result :resource-status))
                      (setq descriptor
                            (plist-put descriptor :result (plist-get result :result)))
                      (setq descriptor
                            (plist-put descriptor :resource-status
                                       (plist-get result :resource-status))))
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
                  (signal 'niwa-resource-error
                          (list "Internal resource error: file-backed resource has no executor")))
                (funcall executor
                         (plist-get data :physical-path)
                         (plist-get data :address))))
          (error
           (signal (car err)
                   (list (niwa-resource-error-message
                          err (plist-get data :address)
                          (list (plist-get data :physical-path)
                                (plist-get data :root)
                                (when-let* ((session (plist-get data :session)))
                                  (niwa-session-save-path session))
                                (when-let* (((eq (plist-get data :scheme) 'notes))
                                            (session (plist-get data :session))
                                            (workspace (niwa-session-workspace session)))
                                  (niwa-workspace-root workspace))
                                (when (eq (plist-get data :scheme) 'work)
                                  (niwa-resource-work-shared-directory
                                   (plist-get data :workspace)))))))))
      (remhash attempt niwa-resource--attempt-table))))

(defun niwa-resource-discard-attempts (attempts)
  "Discard opaque resource ATTEMPTS that will not be executed."
  (dolist (attempt attempts)
    (remhash attempt niwa-resource--attempt-table))
  nil)

(defun niwa-resource-execute-async (attempt callback executor &optional options)
  "Execute authorized ATTEMPT with CALLBACK and asynchronous EXECUTOR.
EXECUTOR receives the ordinary path/descriptor, authored address and final
callback.  Execution spools borrow a disposable logical view owned by
that callback or the returned canceller, not by attempt consumption.  OPTIONS
are the operation's arguments, including any Read pagination."
  (let* ((data (gethash attempt niwa-resource--attempt-table))
         (artifact (eq (plist-get data :scheme) 'artifact))
         (execution (equal (car (plist-get data :components)) "executions"))
         (operation (plist-get data :operation))
         (session (plist-get data :session))
         (resume (niwa-pipeline-handler-resumable
                  (lambda (path address deliver)
                    (let ((niwa--session session))
                      (funcall executor path address deliver))))))
    (niwa-resource-execute
     attempt
     (lambda (path address)
       (let* ((roots (and (listp path) (plist-get path :resource-search-roots)))
              (physical (if roots (plist-get (car roots) :path)
                          (and (stringp path) path)))
              (published-read
               (and execution (eq operation 'read) physical
                    (not (file-directory-p physical))
                    (file-remote-p physical)
                    (eq (niwa-session-authority-mode session) 'portable)
                    (let* ((publication (niwa-session-publication session))
                           (logical (concat "tool-results/"
                                            (string-join (plist-get data :components) "/"))))
                      (or (null publication)
                          (assoc logical (plist-get publication :artifacts))
                          (niwa-session-publication-uncommitted-artifact session logical))))))
         ;; Ordinary portable artifacts keep their publication-aware Read path.
         ;; Only execution files, or directory discovery containing them, borrow
         ;; storage views; a gzip suffix alone never grants that authority.
         ;; Published/staged remote execution bytes still outrank fixed output;
         ;; gzip storage never rescues a failed publication verification.
         (if (not (and artifact physical (not published-read)
                       (or execution (file-directory-p physical))))
             (funcall resume path address callback)
           (niwa-execution-spool-with-path
            physical operation callback
            (lambda (selected deliver)
              (let ((target path))
                (if roots
                    (let ((root (copy-sequence (car roots))))
                      (setq target (copy-sequence path))
                      (plist-put root :path selected)
                      (plist-put target :resource-search-roots (list root)))
                  (setq target selected))
                (funcall resume target address deliver)))
            (when (and (eq operation 'read)
                       (not (file-directory-p physical))
                       (integerp (plist-get options :limit))
                       (> (plist-get options :limit) 0)
                       (or (null (plist-get options :offset))
                           (and (integerp (plist-get options :offset))
                                (> (plist-get options :offset) 0))))
              (+ (or (plist-get options :offset) 1)
                 (plist-get options :limit)))))))
     options)))

(defun niwa-resource-visit-path (address &optional context)
  "Return the file or directory ADDRESS names in CONTEXT, or nil.
Logical resources -- listings, agents, history, MCP and shared items --
have no backing file and return nil, as does an unavailable owner."
  (let ((attempt (niwa-resource-prepare 'read address context)))
    (unwind-protect
        (let ((data (gethash attempt niwa-resource--attempt-table)))
          (and data
               (not (plist-get data :unavailable-p))
               (not (plist-get data :logical-p))
               (let ((path (plist-get data :physical-path)))
                 (if (eq (plist-get data :scheme) 'artifact)
                     (niwa-execution-spool-physical-path path)
                   path))))
      (when attempt (niwa-resource-discard-attempts (list attempt))))))

(defun niwa-resource-current-attempt (address)
  "Return the dynamically active attempt for authored ADDRESS."
  (cdr (assoc address niwa-resource-current-attempts)))

(defun niwa-resource-artifact-address (path session)
  "Return the logical artifact address for PATH owned by SESSION."
  (when-let* ((root (niwa-resource--root 'artifact session))
              (path (expand-file-name path))
              ((niwa-resource-within-root-p path root))
              (relative (niwa-resource--canonical-relative path root))
              ((not (string-match-p
                     "\\`\\.niwa-pending-executions\\(?:/\\|\\'\\)"
                     relative))))
    (concat "artifact://"
            (mapconcat #'niwa-resource-encode-component
                       (split-string relative "/" t)
                       "/"))))

(provide 'niwa-resource)
;;; niwa-resource.el ends here
