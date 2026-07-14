;;; mevedel-mentions.el -- @ref and @file mention system -*- lexical-binding: t -*-

;;; Commentary:

;; @ref/@file expansion, completion-at-point, and font-lock support for
;; mevedel view buffers.  Expansion runs as a gptel prompt transform on the
;; temporary prompt-buffer copy; raw mentions in the view and data buffers are
;; untouched.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-overlays)

;; `gptel'
(declare-function gptel--model-capable-p "ext:gptel-request" (cap &optional model))
(declare-function gptel--model-mime-capable-p "ext:gptel-request" (mime &optional model))
(declare-function gptel-fsm-info "gptel" (fsm))
(defvar gptel-context)
(defvar gptel-use-context)

;; `mcp' / `mcp-hub' (optional dependency)
(declare-function mcp-hub-get-servers "mcp-hub" ())
(declare-function mcp-read-resource "mcp" (connection uri))
(defvar mcp-server-connections)

;; `mevedel-agents'
(declare-function mevedel-agent-description "mevedel-agents" (agent))
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-name "mevedel-agents" (agent))
(defvar mevedel-agent--registry)

;; `mevedel-file-state'
(declare-function mevedel-session-record-file-access
                  "mevedel-file-state" (session path kind &optional offset limit))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-at
                  "mevedel-mention-bindings" (text start end kind token))
(declare-function mevedel-mention-bindings-copy-text
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings"
                  (start end binding &optional object))
(declare-function mevedel-mention-bindings-valid-p
                  "mevedel-mention-bindings" (text))

;; `mevedel-overlays'
(declare-function mevedel--filter-references
                  "mevedel-overlays" (query &optional workspace))
(declare-function mevedel--instruction-activate-buffer
                  "mevedel-overlays" (&optional buffer))
(declare-function mevedel--instruction-alist-value "mevedel-overlays" ())
(declare-function mevedel--instruction-with-id
                  "mevedel-overlays" (target-id &optional workspace))
(declare-function mevedel--instruction-with-uuid
                  "mevedel-overlays" (uuid &optional workspace))

;; `mevedel-permissions'
(declare-function mevedel-check-permission
                  "mevedel-permissions" (tool-name &rest args))

;; `mevedel-persistence'
(declare-function mevedel--restore-file-instructions
                  "mevedel-persistence" (file &optional message))

;; `mevedel-structs'
(declare-function mevedel-session-active-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-mentions-shown "mevedel-structs" (session))
(declare-function mevedel-session-permission-mode "mevedel-structs" (session))
(declare-function mevedel-session-permission-rules "mevedel-structs" (session))
(declare-function mevedel-session-turn-count "mevedel-structs" (session))
(declare-function mevedel-session-workspace "mevedel-structs" (session))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-tool-fs'
(declare-function mevedel-tool-fs--format-large-pdf-reminder
                  "mevedel-tool-fs" (path))
(declare-function mevedel-tool-fs--large-pdf-p "mevedel-tool-fs" (path))
(declare-function mevedel-tool-fs--list-directory
                  "mevedel-tool-fs" (path &optional max-entries))
(declare-function mevedel-tool-fs--media-mime-type
                  "mevedel-tool-fs" (filename))
(declare-function mevedel-tool-fs--pdf-media-p "mevedel-tool-fs" (filename))
(declare-function mevedel-tool-fs--slurp-file-contents
                  "mevedel-tool-fs" (path &optional offset limit))
(defvar mevedel-tool-fs--media-max-bytes)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get
                  "mevedel-tool-registry" (name &optional category))

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
                  "mevedel-transcript" ())

;; `mevedel-utilities'
(declare-function mevedel--delimiting-markdown-backticks
                  "mevedel-utilities" (str))
(declare-function mevedel--markdown-enquote "mevedel-utilities" (text))
(declare-function mevedel--overlay-region-info
                  "mevedel-utilities" (overlay))

;; `mevedel-workspace'
(declare-function mevedel--all-allowed-roots
                  "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))


;;
;;; Reference resolution

(defun mevedel-mentions-replace-with-placeholder (start end placeholder)
  "Replace START..END with PLACEHOLDER, preserving prompt ownership.
When the replaced region is marked with a `gptel' text property, copy
that property to PLACEHOLDER so later prompt transforms keep the same
user-prompt boundary."
  (let ((gptel-prop (get-text-property start 'gptel)))
    (delete-region start end)
    (goto-char start)
    (insert (if gptel-prop
                (propertize placeholder 'gptel gptel-prop)
              placeholder))))

(defun mevedel--resolve-ref-by-id (id &optional workspace)
  "Look up reference by numeric ID in WORKSPACE.
Returns the reference overlay or nil if not found or not a reference."
  (when-let* ((instr (mevedel--instruction-with-id id workspace)))
    (when (mevedel--referencep instr)
      instr)))

(defun mevedel--resolve-ref-by-uuid (uuid &optional workspace)
  "Look up the reference carrying UUID in WORKSPACE, or return nil.
Restore stashed instructions in WORKSPACE before searching live overlays."
  (when-let* ((instr (mevedel--instruction-with-uuid uuid workspace)))
    (and (mevedel--referencep instr) instr)))

(defun mevedel-mentions-prepare-user-input (text &optional session)
  "Bind known direct reference mentions in user TEXT for SESSION.
Existing valid bindings remain authoritative.  Direct numeric mentions
whose current reference exists receive its UUID; missing references and
reference queries remain unbound."
  (require 'mevedel-mention-bindings)
  (unless (mevedel-mention-bindings-valid-p text)
    (user-error "Malformed mention binding"))
  (let ((workspace (and session (mevedel-session-workspace session)))
        (result (mevedel-mention-bindings-copy-text text))
        (position 0))
    (while (string-match "@ref:\\([0-9]+\\)" result position)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (token (match-string 0 result))
             (live-context
              (and (or (= start 0)
                       (memq (char-syntax (aref result (1- start)))
                             '(?\s ?>)))
                   (or (= end (length result))
                       (not (string-match-p
                             "[[:alnum:]_]"
                             (char-to-string (aref result end))))))))
        (when (and live-context
                   (not (get-text-property
                         start 'mevedel-mention-binding result)))
          (when-let* ((ref (mevedel--resolve-ref-by-id
                            (string-to-number (match-string 1 result))
                            workspace))
                      (uuid (overlay-get ref 'mevedel-uuid)))
            (mevedel-mention-bindings-set
             start end
             (list :kind 'ref :token token :reference-uuid uuid)
             result)))
        (setq position end)))
    result))

(defun mevedel--resolve-refs-by-tag-query (query-string &optional workspace)
  "Filter references by tag QUERY-STRING in WORKSPACE.
Returns list of reference overlays matching the query.
For @ref mentions, excludes untagged references even if
`mevedel-always-match-untagged-references' is t."
  (condition-case _err
      (let* ((query (mevedel--tag-query-prefix-from-infix
                     (read (concat "(" query-string ")"))))
             ;; Explicit @ref queries never include untagged references.
             (mevedel-always-match-untagged-references nil)
             (refs (mevedel--filter-references query workspace)))
        refs)
    (error nil)))

(defun mevedel--format-single-reference (ref)
  "Format a single reference REF as markdown.
Returns a string with the reference header and content."
  (cl-destructuring-bind (ref-info-string ref-string)
      (mevedel--overlay-region-info ref)
    (let ((markdown-delimiter
           (mevedel--delimiting-markdown-backticks ref-string))
          (rel-path (file-relative-name
                     (buffer-file-name (overlay-buffer ref))
                     (mevedel-workspace-root (mevedel-workspace)))))
      (concat
       (format "#### Reference #%d\n\n" (mevedel--instruction-id ref))
       (if (mevedel--instruction-bufferlevel-p ref)
           (format "File `%s`." rel-path)
         (format "In file `%s`, %s" rel-path ref-info-string))
       (if (or (mevedel--instruction-bufferlevel-p ref)
               (not mevedel-include-full-instructions))
           "."
         (concat
          ":"
          (format "\n\n%s\n%s\n%s"
                  markdown-delimiter
                  ref-string
                  markdown-delimiter)))
       (let ((commentary (mevedel--commentary-text ref)))
         (if (string-empty-p commentary)
             ""
           (format "\n\nCommentary:\n\n%s"
                   (mevedel--markdown-enquote commentary))))))))


;;
;;; Customization

(defgroup mevedel-mentions nil
  "Customization for mevedel's @ref and @file mention system."
  :group 'mevedel)

(defcustom mevedel-file-mention-directory-max-entries 1000
  "Maximum number of entries included in an @file directory listing.
When a directory has more entries than this, the listing is truncated
and the reminder notes the truncation so the LLM can use Glob or Grep
to drill down instead."
  :type 'integer
  :group 'mevedel-mentions)

(defconst mevedel-mentions--file-regexp
  "@file:\\({\\(?:\\\\.\\|[^}]\\)+}\\|[^ \t\n#]+\\)\\(?:#L\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\)?"
  "Regexp matching an @file mention.
Capture group 1 is the bare path or the braced path token.  Capture
groups 2 and 3 are optional line-range bounds.")

(defconst mevedel-mentions--mcp-regexp
  "@mcp:\\([^: \t\n]+\\):\\(\\S-+\\)"
  "Regexp matching an @mcp resource mention.")


;;
;;; Mention dispatch

(defvar mevedel-mention-handlers
  `(("@ref:\\(?:\\([0-9]+\\)\\|{\\([^}]+\\)}\\)"
     ref mevedel--handle-ref-mention)
    ("@agent:\\([[:alnum:]_-]+\\)" nil mevedel--handle-agent-mention)
    (,mevedel-mentions--mcp-regexp mcp mevedel--handle-mcp-mention)
    (,mevedel-mentions--file-regexp file mevedel--handle-file-mention))
  "List of mention regex, binding kind, and handler descriptors.

Each handler is called with a plist containing:
  :match-text       the full matched text
  :capture          the first regex capture group
  :binding          the exact atomic binding at the match, or nil
  :session          the `mevedel-session' struct (nil if unavailable)
  :workspace-root   the workspace root directory (nil if unavailable)

Handlers return a plist with these keys:
  :placeholder  inline replacement text for the raw mention
  :reminder     content to emit as a <system-reminder> block, or nil
  :media-context (PATH MIME) context attachment for gptel media, or nil
  :key          (KIND . KEY) identifier used for deduplication
  :hash         content hash used for deduplication, or nil
  :warning      nonblocking user-facing warning text, or nil

Handlers should always return a `:placeholder', even for missing or
invalid references, so that the user's prompt never leaks raw mention
syntax to the LLM.")

(defun mevedel--handle-ref-mention (info)
  "Handler for @ref:ID and @ref:{tag-query} mentions.
See `mevedel-mention-handlers' for the INFO plist shape.  The combined
regex exposes two capture groups: group 1 is the numeric ID variant,
group 2 is the tag-query variant.  Exactly one is non-nil per match."
  (let* ((captures (plist-get info :captures))
         (id-str (nth 1 captures))
         (query (nth 2 captures))
         (binding (plist-get info :binding))
         (session (plist-get info :session))
         (workspace (and session (mevedel-session-workspace session))))
    (cond
     (id-str
      (let* ((id (string-to-number id-str))
             (uuid (and (eq 'ref (plist-get binding :kind))
                        (plist-get binding :reference-uuid)))
             (ref (if uuid
                      (mevedel--resolve-ref-by-uuid uuid workspace)
                    (mevedel--resolve-ref-by-id id workspace))))
        (if ref
            (let ((content (with-current-buffer (overlay-buffer ref)
                             (buffer-substring-no-properties
                              (overlay-start ref) (overlay-end ref)))))
              (list :placeholder (format "[ref:%d -- contents attached above]" id)
                    :reminder (mevedel--format-single-reference ref)
                    :key (cons 'ref (overlay-get ref 'mevedel-uuid))
                    :hash (secure-hash 'sha1 content)))
          (if uuid
              (list :placeholder (format "[ref:%d -- unavailable]" id)
                    :reminder
                    (format "The user selected reference #%d, but that exact \
reference is unavailable.  The `[ref:%d -- unavailable]' token in the user \
prompt is a system annotation, not user-written text.  Do not infer another \
reference from the displayed number." id id)
                    :key (cons 'ref uuid)
                    :hash nil
                    :warning (format "reference @ref:%d is unavailable" id))
            (list :placeholder
                  (format "[ref:%d -- removed since an earlier turn]" id)
                  :reminder
                  (format "The user referenced reference #%d via an @ref mention, \
but that reference no longer exists.  The `[ref:%d -- removed since an \
earlier turn]' token in the user prompt is a system annotation, not \
user-written text.  Do not mention this reminder to the user." id id)
                  :key (cons 'ref id)
                  :hash nil)))))
     (query
      (let ((refs (mevedel--resolve-refs-by-tag-query query workspace)))
        (if (null refs)
            (list :placeholder (format "[ref:{%s} -- no matches]" query)
                  :reminder
                  (format "The user queried references matching `%s` via an \
@ref:{...} mention, but no references matched.  The `[ref:{%s} -- no matches]' \
token in the user prompt is a system annotation, not user-written text.  \
Do not mention this reminder to the user."
                          query query)
                  :key (cons 'ref-tag query)
                  :hash nil)
          (let* ((sorted (sort (copy-sequence refs)
                               (lambda (a b)
                                 (< (mevedel--instruction-id a)
                                    (mevedel--instruction-id b)))))
                 (ids (mapconcat
                       (lambda (r) (format "#%d" (mevedel--instruction-id r)))
                       sorted ", "))
                 (content-for-hash
                  (mapconcat
                   (lambda (r)
                     (format "%d:%s"
                             (mevedel--instruction-id r)
                             (with-current-buffer (overlay-buffer r)
                               (buffer-substring-no-properties
                                (overlay-start r) (overlay-end r)))))
                   sorted "|"))
                 (reminder (concat (format "References matching `%s`:\n\n" query)
                                   (mapconcat #'mevedel--format-single-reference
                                              sorted "\n\n"))))
            (list :placeholder
                  (format "[refs matching '%s': %s -- contents attached above]"
                          query ids)
                  :reminder reminder
                  :key (cons 'ref-tag query)
                  :hash (secure-hash 'sha1 content-for-hash)))))))))

(defun mevedel--handle-agent-mention (info)
  "Handler for @agent:name mentions.
See `mevedel-mention-handlers' for the INFO plist shape.

When NAME matches a registered agent, returns a reminder instructing
the main agent to delegate the rest of this turn via the `Agent' tool.
When NAME is unknown, returns a graceful-failure placeholder."
  (let* ((name (plist-get info :capture))
         (agent (and (fboundp 'mevedel-agent-get) (mevedel-agent-get name))))
    (if (not agent)
        (list :placeholder (format "[agent:%s -- no such agent]" name)
              :reminder
              (format "The user referenced agent `%s` via an @agent \
mention, but no agent by that name is registered.  The `[agent:%s -- \
no such agent]' token in the user prompt is a system annotation, \
not user-written text.  Do not mention this reminder to the user."
                      name name)
              :key (cons 'agent name)
              :hash nil)
      (list :placeholder (format "[agent:%s -- delegation requested]" name)
            :reminder
            (format "The user asked you to delegate this turn to the \
`%s` sub-agent (%s) via the `Agent` tool.  Invoke \
`Agent(subagent_type=\"%s\", prompt=...)' with a restatement of the \
user's task; do not answer directly.  The `[agent:%s -- delegation \
requested]' token in the user prompt is a system annotation, not \
user-written text.  Do not mention this reminder to the user."
                    name
                    (or (mevedel-agent-description agent) "no description")
                    name name)
            :key (cons 'agent name)
            :hash (secure-hash 'sha1 name)))))

(defun mevedel--mcp-extract-resource-text (res)
  "Extract concatenated text content from an MCP resources/read RES plist.
Returns a string joined from each `:contents' entry's `:text' slot."
  (let ((contents (plist-get res :contents)))
    (string-join
     (delq nil
           (mapcar (lambda (c)
                     (let ((text (plist-get c :text)))
                       (and text (stringp text) text)))
                   (if (vectorp contents) (append contents nil) contents)))
     "\n")))

(defun mevedel--handle-mcp-mention (info)
  "Handler for @mcp:server:uri mentions.
See `mevedel-mention-handlers' for the INFO plist shape.  Reads the
resource via `mcp-read-resource' when the server is configured and
connected; otherwise emits a graceful placeholder."
  (let* ((captures (plist-get info :captures))
         (server (nth 1 captures))
         (uri (nth 2 captures))
         (display (format "%s:%s" server uri))
         (deny
          (lambda (msg)
            (list :placeholder (format "[mcp:%s -- %s]" display msg)
                  :reminder
                  (format "The user referenced MCP resource `%s:%s` via \
an @mcp mention, but the attachment was rejected: %s.  The resource \
contents are NOT available in this turn; the `[mcp:%s -- %s]' token in \
the user prompt is a system annotation, not user-written text.  Do not \
mention this reminder to the user."
                          server uri msg display msg)
                  :key (cons 'mcp (cons server uri))
                  :hash nil))))
    (cond
     ((not (and (featurep 'mcp) (fboundp 'mcp-hub-get-servers)))
      (funcall deny "mcp.el not available"))
     (t
      (let* ((servers (mcp-hub-get-servers))
             (server-info (seq-find
                           (lambda (s)
                             (equal (plist-get s :name) server))
                           servers)))
        (cond
         ((null server-info)
          (funcall deny (format "unknown server `%s`" server)))
         ((not (eq 'connected (plist-get server-info :status)))
          (funcall deny (format "server `%s` not connected" server)))
         (t
          (let ((connection (and (boundp 'mcp-server-connections)
                                 mcp-server-connections
                                 (gethash server mcp-server-connections))))
            (if (not connection)
                (funcall deny (format "no active connection to `%s`" server))
              (condition-case err
                  (let* ((res (mcp-read-resource connection uri))
                         (content (mevedel--mcp-extract-resource-text res))
                         (body (if (string-empty-p content)
                                   "(resource returned no text content)"
                                 content))
                         (hash (secure-hash 'sha1 body)))
                    (list :placeholder
                          (format "[mcp:%s -- contents attached above]"
                                  display)
                          :reminder
                          (format "MCP resource `%s` from server `%s` \
(attached by @mcp mention):\n\n```\n%s\n```"
                                  uri server body)
                          :key (cons 'mcp (cons server uri))
                          :hash hash))
                (error
                 (funcall deny
                          (format "read failed: %s"
                                  (error-message-string err))))))))))))))

(defun mevedel-mentions--unescape-braced-file-path (token)
  "Return TOKEN decoded as a braced @file path.
TOKEN may be either a bare path or a `{...}' path.  Inside braces, a
backslash quotes the following character."
  (if (not (and (string-prefix-p "{" token)
                (string-suffix-p "}" token)))
      token
    (let* ((body (substring token 1 -1))
           (index 0)
           (limit (length body))
           chars)
      (while (< index limit)
        (let ((ch (aref body index)))
          (if (and (= ch ?\\) (< (1+ index) limit))
              (progn
                (cl-incf index)
                (push (aref body index) chars))
            (push ch chars)))
        (cl-incf index))
      (apply #'string (nreverse chars)))))

(defun mevedel-mentions--file-path-from-captures (info)
  "Return the @file path represented by INFO."
  (or (when-let* ((captures (plist-get info :captures))
                  (token (nth 1 captures)))
        (mevedel-mentions--unescape-braced-file-path token))
      (plist-get info :capture)))

(defun mevedel-mentions-file-paths-in-text (text)
  "Return expanded @file paths mentioned in TEXT.
The parser accepts both bare `@file:path' and braced
`@file:{path with spaces}' forms and observes the normal mention
boundary checks."
  (let (paths)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward mevedel-mentions--file-regexp nil t)
        (when (mevedel-mentions--valid-mention-context-p (match-beginning 0))
          (push (expand-file-name
                 (mevedel-mentions--unescape-braced-file-path
                  (match-string 1)))
                paths))))
    (nreverse paths)))

(defun mevedel-mentions--file-content-hash (path)
  "Return a SHA1 hash of PATH's literal file contents."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha1 (buffer-string))))

(defun mevedel-mentions--media-supported-p (mime)
  "Return non-nil when the active gptel model supports MIME."
  (and (fboundp 'gptel--model-capable-p)
       (fboundp 'gptel--model-mime-capable-p)
       (ignore-errors
         (and (gptel--model-capable-p 'media)
              (gptel--model-mime-capable-p mime)))))

(defun mevedel-mentions--add-media-context (path mime)
  "Add PATH with MIME to this prompt buffer's gptel media context."
  (unless (local-variable-p 'gptel-context)
    (setq-local gptel-context (copy-sequence gptel-context)))
  (cl-pushnew (list path :mime mime) gptel-context :test #'equal)
  (unless gptel-use-context
    (setq-local gptel-use-context 'system)))

(defun mevedel-mentions--allowed-roots (info)
  "Return allowed roots for INFO."
  (let ((chat-buffer (plist-get info :chat-buffer))
        (workspace-root (plist-get info :workspace-root)))
    (or (and chat-buffer
             (buffer-live-p chat-buffer)
             (fboundp 'mevedel--all-allowed-roots)
             (ignore-errors
               (mevedel--all-allowed-roots chat-buffer)))
        (and workspace-root (list workspace-root)))))

(defun mevedel--handle-file-mention (info)
  "Handler for @file:path mentions.
See `mevedel-mention-handlers' for the INFO plist shape.

The INFO :captures list for this handler is (WHOLE PATH START END), where
PATH is either a bare path or a braced token, and START and END are
optional strings from the `#L<start>[-<end>]' suffix."
  (let* ((path (mevedel-mentions--file-path-from-captures info))
         (captures (plist-get info :captures))
         (start-str (nth 2 captures))
         (end-str (nth 3 captures))
         (start-line (and start-str (string-to-number start-str)))
         (end-line (and end-str (string-to-number end-str)))
         (range-label
          (cond ((and start-line end-line) (format "#L%d-%d" start-line end-line))
                (start-line (format "#L%d" start-line))
                (t "")))
         (offset start-line)
         (limit (cond ((and start-line end-line)
                       (max 1 (1+ (- end-line start-line))))
                      (start-line 1)
                      (t nil)))
         (expanded (expand-file-name path))
         (session (plist-get info :session))
         (workspace-root (plist-get info :workspace-root))
         (session-rules (and session
                             (mevedel-session-permission-rules session)))
         (exact-allowed-paths
          (and session
               (mevedel-session-active-dropped-file-grants session)))
         (display-path (concat path range-label))
         (dedup-key (cons 'file (cons expanded range-label)))
         (deny-placeholder
          (lambda (msg &optional guidance)
            (list :placeholder (format "[file:%s -- %s]" display-path msg)
                  :reminder
                  (concat
                   (format "The user referenced `%s` via an @file mention, \
but the attachment was rejected: %s.  The file contents are NOT available \
in this turn; the `[file:... -- %s]' token in the user prompt is a system \
annotation, not user-written text.  %s Do not mention this reminder \
to the user."
                           display-path msg msg
                           (if guidance
                               "Do not assume the file contents are available in this turn."
                             "Do not attempt to read the file unless the user explicitly asks you to."))
                   (when guidance
                     (concat "\n\n" guidance)))
                  :key dedup-key
                  :hash nil))))
    (cond
     ((not (file-exists-p expanded))
      (funcall deny-placeholder "does not exist"))
     ((not (file-readable-p expanded))
      (funcall deny-placeholder "unreadable"))
     ((not (eq 'allow
               (mevedel-check-permission
                "Read"
                :tool-struct (ignore-errors (mevedel-tool-get "Read"))
                :path expanded
                :session-rules session-rules
                :mode (and session (mevedel-session-permission-mode session))
                :workspace-root workspace-root
                :allowed-roots (mevedel-mentions--allowed-roots info)
                :exact-allowed-paths exact-allowed-paths)))
      (funcall deny-placeholder "permission denied"))
     ((file-directory-p expanded)
      (condition-case err
          (let* ((listing (mevedel-tool-fs--list-directory
                           expanded
                           mevedel-file-mention-directory-max-entries))
                 (entries (car listing))
                 (truncated (cdr listing))
                 (body (if entries (mapconcat #'identity entries "\n")
                         "(empty or .gitignore filters out all entries)"))
                 (hash (secure-hash 'sha1 body))
                 (truncation-note
                  (if truncated
                      (format "\n\n(Listing truncated at %d entries; \
use Glob or Grep to drill down further.)"
                              mevedel-file-mention-directory-max-entries)
                    "")))
            (list :placeholder
                  (format "[file:%s -- directory listing attached above]"
                          display-path)
                  :reminder
                  (format "Directory listing of `%s` (%d entr%s, \
gitignore-filtered):\n\n```\n%s\n```%s"
                          path
                          (length entries)
                          (if (= (length entries) 1) "y" "ies")
                          body
                          truncation-note)
                  :key (cons 'dir expanded)
                  :hash hash))
        (error
         (funcall deny-placeholder (error-message-string err)))))
     ((mevedel-tool-fs--media-mime-type expanded)
      (let ((mime (mevedel-tool-fs--media-mime-type expanded)))
        (cond
         ((or offset limit)
          (funcall deny-placeholder "line ranges are not supported for media"))
         ((not (mevedel-mentions--media-supported-p mime))
          (funcall deny-placeholder
                   (format "model does not support %s media" mime)))
         ((> (file-attribute-size (file-attributes expanded))
             mevedel-tool-fs--media-max-bytes)
          (funcall deny-placeholder
                   "media file is too large"
                   (when (mevedel-tool-fs--pdf-media-p expanded)
                     (mevedel-tool-fs--format-large-pdf-reminder
                      expanded))))
         (t
          (let ((reminder
                 (and (mevedel-tool-fs--pdf-media-p expanded)
                      (mevedel-tool-fs--large-pdf-p expanded)
                      (mevedel-tool-fs--format-large-pdf-reminder
                       expanded))))
            (list :placeholder
                  (format "[file:%s -- media attached]" display-path)
                  :reminder reminder
                  :media-context (list expanded mime)
                  :key dedup-key
                  :hash (mevedel-mentions--file-content-hash expanded)))))))
     (t
      (condition-case err
          (let* ((content (mevedel-tool-fs--slurp-file-contents
                           expanded offset limit))
                 (hash (secure-hash 'sha1 content))
                 (range-phrase
                  (cond ((and start-line end-line)
                         (format "lines %d-%d of " start-line end-line))
                        (start-line
                         (format "line %d of " start-line))
                        (t "contents of "))))
            (when (and session (null offset) (null limit))
              (mevedel-session-record-file-access
               session expanded 'read nil nil))
            (list :placeholder
                  (format "[file:%s -- contents attached above]" display-path)
                  :reminder
                  (format "%s`%s` (attached by @file mention).  \
Do not call Read on this file again unless you need a different range.\n\n\
```\n%s\n```"
                          (capitalize range-phrase) path content)
                  :key dedup-key
                  :hash hash))
        (error
         (funcall deny-placeholder (error-message-string err))))))))

(defun mevedel-mentions--valid-mention-context-p (match-beg)
  "Return non-nil when a mention starting at MATCH-BEG should be live.

A mention is considered live when both of the following hold:

  1. It is not inside a gptel-owned region.  Specifically, rejects
     `gptel' text-property values `response', `ignore', and `(tool
     . _)' — prior assistant replies, explicitly-ignored content,
     and tool-call sexps.  Anything else (nil, `prompt', unknown
     values) is treated as user-authored text.

  2. It is at a word boundary — preceded by whitespace, a newline,
     or the start of the buffer — so code spans like
     \\=`@file:foo\\=' and quoted forms like \"@file:foo\" are
     treated as references to the syntax, not live invocations."
  (let ((g (get-text-property match-beg 'gptel)))
    (and (not (memq g '(response ignore)))
         (not (and (consp g) (eq (car g) 'tool)))
         (or (= match-beg (point-min))
             (memq (char-syntax (char-before match-beg))
                   '(?\s ?>))))))

(defun mevedel--transform-expand-mentions (fsm)
  "GPtel transform function expanding every mention type.

Walks the whole prompt buffer, replacing each raw mention with a
compact placeholder.  For each novel mention (first occurrence in the
chat, or whose content hash differs from what was last shown), inserts
a <system-reminder> block immediately before the last user prompt.
Mentions whose key and content hash match what the session's
`mentions-shown' table already recorded are replaced with the
placeholder but not re-expanded as reminders.

FSM is the gptel finite-state machine; its info plist's :buffer entry
points back at the chat buffer that owns the session.  Dispatches per
`mevedel-mention-handlers'."
  (let* ((chat-buffer (and fsm (plist-get (gptel-fsm-info fsm) :buffer)))
         (session (and chat-buffer (buffer-live-p chat-buffer)
                       (buffer-local-value 'mevedel--session chat-buffer)))
         (workspace-root
          (and session
               (when-let* ((ws (mevedel-session-workspace session)))
                 (mevedel-workspace-root ws))))
         (mentions-shown (and session
                              (mevedel-session-mentions-shown session)))
         (turn (and session (mevedel-session-turn-count session)))
         (seen-this-pass (make-hash-table :test #'equal))
         (new-items nil)
         (warnings nil))
    (require 'mevedel-mention-bindings)
    (dolist (entry mevedel-mention-handlers)
      (let ((regex (car entry))
            (kind (nth 1 entry))
            (handler (nth 2 entry)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regex nil t)
            (let ((match-beg (match-beginning 0))
                  (match-end (match-end 0)))
              (when (mevedel-mentions--valid-mention-context-p match-beg)
                (let* ((match-text (match-string 0))
                       (binding
                        (and kind
                             (let* ((context-start
                                     (max (point-min) (1- match-beg)))
                                    (context-end
                                     (min (point-max) (1+ match-end)))
                                    (context
                                     (buffer-substring
                                      context-start context-end)))
                               (mevedel-mention-bindings-at
                                context
                                (- match-beg context-start)
                                (- match-end context-start)
                                kind match-text))))
                       (captures (cl-loop for i from 0 below
                                          (/ (length (match-data)) 2)
                                          collect (match-string i)))
                       (info (list :match-text match-text
                                   :capture (match-string 1)
                                   :captures captures
                                   :binding binding
                                   :session session
                                   :workspace-root workspace-root
                                   :chat-buffer chat-buffer))
                       (result (funcall handler info))
                       (placeholder (plist-get result :placeholder))
                       (reminder (plist-get result :reminder))
                       (media-context (plist-get result :media-context))
                       (key (plist-get result :key))
                       (hash (plist-get result :hash))
                       (warning (plist-get result :warning))
                       (prior (and mentions-shown key
                                   (gethash key mentions-shown)))
                       (already-sent-same (and prior hash
                                               (equal (cdr prior) hash)))
                       (seen-key-p (and key (gethash key seen-this-pass)))
                       (fresh-reminder-p
                        (and reminder
                             (or (null key)
                                 (and (not seen-key-p)
                                      (not already-sent-same))))))
                  (mevedel-mentions-replace-with-placeholder
                   match-beg match-end placeholder)
                  (when warning
                    (push warning warnings))
                  (when media-context
                    (apply #'mevedel-mentions--add-media-context
                           media-context))
                  (when (or fresh-reminder-p media-context)
                    (when (and key (or fresh-reminder-p media-context))
                      (puthash key t seen-this-pass))
                    (push (list :key key
                                :hash hash
                                :reminder (and fresh-reminder-p reminder))
                          new-items)))))))))
    (when warnings
      (message "mevedel: %s"
               (mapconcat #'identity (delete-dups (nreverse warnings)) "; ")))
    (when-let* ((reminder-items
                 (delq nil
                       (mapcar (lambda (item)
                                 (and (plist-get item :reminder)
                                      item))
                               new-items))))
      (goto-char (mevedel-transcript-prompt-transform-start))
      (save-excursion
        (dolist (item (nreverse reminder-items))
          (let ((start (point)))
            (insert "<system-reminder>\n"
                    (plist-get item :reminder)
                    "\n</system-reminder>\n\n")
            (remove-text-properties
             start (point)
             '(gptel nil response nil invisible nil front-sticky nil))))))
    (when (and mentions-shown turn)
      (dolist (item new-items)
        (when (and (plist-get item :key)
                   (plist-get item :hash))
          (puthash (plist-get item :key)
                   (cons turn (plist-get item :hash))
                   mentions-shown))))))


;;
;;; Completion-at-point support for @ref mentions

(defun mevedel-mentions--ref-completion-exit-function
    (candidates candidate status)
  "Bind completed reference CANDIDATE selected from CANDIDATES.
STATUS is the completion exit status."
  (when (memq status '(finished exact sole))
    (when-let* ((selected (cl-find candidate candidates :test #'string=))
                (uuid (get-text-property
                       0 'mevedel-reference-uuid selected))
                (id-start (- (point) (length candidate)))
                (start (- id-start (length "@ref:")))
                ((>= start (point-min)))
                (token (concat "@ref:" candidate))
                ((equal token
                        (buffer-substring-no-properties start (point)))))
      (require 'mevedel-mention-bindings)
      (mevedel-mention-bindings-set
       start (point)
       (list :kind 'ref :token token :reference-uuid uuid)))))

(defun mevedel-ref-capf ()
  "Completion-at-point function for @ref mentions.
Provides completion for both @ref:ID and @ref:{tag-query} syntax."
  (mevedel--instruction-activate-buffer)
  (when (mevedel--instruction-alist-value)
    (save-excursion
      (let ((orig-point (point)))
        ;; Try to match @ref:ID pattern
        (if (and (skip-chars-backward "0-9")
                 (looking-back "@ref:" (- (point) 5)))
            (let* ((start (point))
                   (end (+ start (skip-chars-forward "0-9")))
                   ;; Build completion table from all reference IDs
                   (candidates
                    (delq nil
                          (mapcar
                           (lambda (ref)
                             (let* ((id (mevedel--instruction-id ref))
                                    (id-str (number-to-string id))
                                    (buffer (overlay-buffer ref))
                                    (file-name (buffer-file-name buffer))
                                    (rel-path (when file-name
                                                (file-relative-name
                                                 file-name
                                                 (mevedel-workspace-root (mevedel-workspace)))))
                                    (line (with-current-buffer buffer
                                            (line-number-at-pos (overlay-start ref))))
                                    (content (with-current-buffer buffer
                                               (buffer-substring-no-properties
                                                (overlay-start ref)
                                                (overlay-end ref))))
                                    (preview (truncate-string-to-width
                                              (string-trim (replace-regexp-in-string "[\n\r]+" " " content))
                                              50 nil nil "...")))
                                ;; Store reference info as text property for annotation
                               (propertize id-str
                                           'mevedel-reference-uuid
                                           (overlay-get ref 'mevedel-uuid)
                                           'mevedel-file rel-path
                                           'mevedel-line line
                                           'mevedel-preview preview)))
                           (mevedel--foreach-instruction instr
                                                         when (mevedel--referencep instr)
                                                         collect instr)))))
              (list start end candidates
                    :exclusive 'no
                    :annotation-function
                    #'(lambda (cand)
                        (let ((file (get-text-property 0 'mevedel-file cand))
                              (line (get-text-property 0 'mevedel-line cand))
                              (preview (get-text-property 0 'mevedel-preview cand)))
                          (format " %s [%s:%d]" preview file line)))
                    :exit-function
                    (lambda (candidate status)
                      (mevedel-mentions--ref-completion-exit-function
                       candidates candidate status))))

          ;; Try to match @ref:{tag-query} pattern
          (goto-char orig-point)
          (when (looking-back "@ref:{[^}]*" (line-beginning-position))
            (let* ((start (save-excursion
                            (search-backward "{")
                            (1+ (point))))
                   (end (save-excursion
                          (skip-chars-forward "^}")
                          (point)))
                   ;; Build completion table from all tags
                   (candidates
                    (let ((all-tags (mevedel--available-tags)))
                      ;; Return tag completions with match counts
                      (mapcar
                       (lambda (tag)
                         ;; Convert tag to string if it's a symbol
                         (let* ((tag-str (if (symbolp tag) (symbol-name tag) tag))
                                (refs (mevedel--resolve-refs-by-tag-query tag-str))
                                (count (length refs)))
                           (propertize tag-str
                                       'mevedel-match-count count)))
                       all-tags))))
              (list start end candidates
                    :exclusive 'no
                    :annotation-function
                    (lambda (cand)
                      (let ((count (get-text-property 0 'mevedel-match-count cand)))
                        (format " [%d ref%s]" count (if (= count 1) "" "s"))))))))))))

(defun mevedel-file-capf ()
  "Completion-at-point function for @file: mentions.
Provides hierarchical directory-by-directory file completion.
When a file is selected, replaces @file:path with the absolute path."
  (save-excursion
    (let ((orig-point (point)))
      ;; Look back to find @file: pattern
      (when (re-search-backward "@file:" (line-beginning-position) t)
        (let* ((prefix-start (point))  ; Start of @file:
               (path-start (+ prefix-start 6))  ; Position after @file:
               (path-end (progn
                           (goto-char orig-point)
                           (skip-chars-forward "^ \t\n")
                           (point)))
               (current-input (buffer-substring-no-properties path-start path-end))
               (workspace (mevedel-workspace))
               (workspace-root (when workspace (mevedel-workspace-root workspace))))
          (when workspace-root
            ;; Parse current input to determine directory context
            (let* ((dir-part (file-name-directory current-input))
                   (current-dir (expand-file-name (or dir-part "") workspace-root))
                   ;; Get immediate children of current directory
                   (candidates
                    (when (file-directory-p current-dir)
                      (let* ((entries (directory-files current-dir nil "^[^.]"))
                             (file-entries
                              (delq nil
                                    (mapcar
                                     (lambda (entry)
                                       (let* ((full-path (expand-file-name entry current-dir))
                                              (is-dir (file-directory-p full-path)))
                                         ;; Skip excluded directories
                                         (unless (and is-dir
                                                      (member entry '(".git" ".svn" "node_modules"
                                                                      ".venv" "__pycache__" "build" "dist")))
                                           ;; Construct candidate in context of current input
                                           (propertize (concat (or dir-part "")
                                                               entry
                                                               (when is-dir "/"))
                                                       'mevedel-abs-path full-path
                                                       'mevedel-is-dir is-dir))))
                                     entries)))
                             (parent-dir (expand-file-name ".." current-dir))
                             ;; Add "." to represent current directory (for finalizing on it)
                             (current-dir-entry
                              (when dir-part  ; Only show when we're in a subdirectory
                                (propertize (concat (or dir-part "") ".")
                                            'mevedel-abs-path current-dir
                                            'mevedel-is-dir 'current))))
                        ;; Build final candidate list: special entries + file entries
                        (append (delq nil
                                      (list
                                       ;; Add .. unless at filesystem root
                                       (when (not (string= current-dir "/"))
                                         (propertize (concat (or dir-part "") "../")
                                                     'mevedel-abs-path parent-dir
                                                     'mevedel-is-dir t))
                                       ;; Add . to finalize on current directory
                                       current-dir-entry))
                                file-entries)))))
              (when candidates
                (list path-start path-end candidates
                      :exclusive 'no
                      :exit-function
                      (lambda (str status)
                        ;; Replace @file:path with absolute path for:
                        ;; 1. Files (not directories) - always expand
                        ;; 2. Special "." entry (current dir) - expand to finalize on directory
                        ;; Regular directories don't expand to allow navigation
                        (when (memq status '(finished sole exact))
                          (let ((abs-path (get-text-property 0 'mevedel-abs-path str))
                                (is-dir (get-text-property 0 'mevedel-is-dir str)))
                            ;; Convert to absolute if: it's a file OR it's the current dir marker
                            (when (and abs-path
                                       (or (not is-dir)              ; Files
                                           (eq is-dir 'current)))    ; Current dir marker "."
                              ;; Delete path portion (preserving @file: prefix) and
                              ;; insert absolute path
                              (let ((end-pos (point)))
                                (delete-region path-start end-pos)
                                (insert abs-path)
                                ;; Add trailing / for current directory marker
                                (when (and (eq is-dir 'current)
                                           (not (eq (char-before) ?/)))
                                  (insert "/")))))))
                      :annotation-function
                      (lambda (cand)
                        (let ((is-dir (get-text-property 0 'mevedel-is-dir cand)))
                          (cond
                           ((eq is-dir 'current) " [current dir]")
                           (is-dir " [dir]")
                           (t " [file]")))))))))))))


;;
;;; Font-lock support for @ref mentions

(defun mevedel-mentions--fontify-keyword (regexp end)
  "Search forward for a valid mention matching REGEXP before END."
  (let (found)
    (while (and (not found)
                (re-search-forward regexp end t))
      (when (mevedel-mentions--valid-mention-context-p (match-beginning 0))
        (setq found t)))
    found))

(defun mevedel--fontify-ref-id-keyword (end)
  "Font-lock matcher for @ref:ID mentions up to END.
Highlights valid reference IDs.  Skips mentions in non-user regions
or adjacent to quoting chars; see
`mevedel-mentions--valid-mention-context-p'."
  (mevedel-mentions--fontify-keyword "@ref:\\([0-9]+\\)" end))

(defun mevedel--fontify-ref-tag-keyword (end)
  "Font-lock matcher for @ref:{tag} mentions up to END.
Highlights valid tag queries.  Skips mentions in non-user regions
or adjacent to quoting chars."
  (mevedel-mentions--fontify-keyword "@ref:{\\([^}]+\\)}" end))

(defun mevedel--fontify-file-keyword (end)
  "Font-lock matcher for @file:path mentions up to END.
Highlights file path references, including optional `#L<start>[-<end>]'
line-range suffix.  Skips mentions in non-user regions or adjacent
to quoting chars."
  (mevedel-mentions--fontify-keyword mevedel-mentions--file-regexp end))

(defun mevedel--fontify-agent-keyword (end)
  "Font-lock matcher for @agent:name mentions up to END.
Skips mentions in non-user regions or adjacent to quoting chars."
  (mevedel-mentions--fontify-keyword "@agent:\\([[:alnum:]_-]+\\)" end))

(defun mevedel--fontify-mcp-keyword (end)
  "Font-lock matcher for @mcp:server:uri mentions up to END.
Skips mentions in non-user regions or adjacent to quoting chars."
  (mevedel-mentions--fontify-keyword
   mevedel-mentions--mcp-regexp end))

(defconst mevedel-mentions--font-lock-keywords
  '((mevedel--fontify-ref-id-keyword
     0 (let ((id (string-to-number (match-string 1))))
         (if (mevedel--resolve-ref-by-id id)
             '(:box (:line-width -1) :inherit success)
           '(:box (:line-width -1) :inherit shadow)))
     prepend)
    (mevedel--fontify-ref-tag-keyword
     0 (let* ((query (match-string 1))
              (refs (mevedel--resolve-refs-by-tag-query query)))
         (if (and refs (> (length refs) 0))
             '(:box (:line-width -1) :inherit success)
           '(:box (:line-width -1) :inherit shadow)))
     prepend)
    (mevedel--fontify-file-keyword
     0 (let ((filepath (mevedel-mentions--unescape-braced-file-path
                        (match-string 1))))
         (if (file-exists-p filepath)
             '(:box (:line-width -1) :inherit link)
           '(:box (:line-width -1) :inherit shadow)))
     prepend)
    (mevedel--fontify-agent-keyword
     0 (let ((name (match-string 1)))
         (if (and (fboundp 'mevedel-agent-get) (mevedel-agent-get name))
             '(:box (:line-width -1) :inherit success)
           '(:box (:line-width -1) :inherit shadow)))
     prepend)
    (mevedel--fontify-mcp-keyword
     0 (let ((server (match-string 1)))
         (if (and (featurep 'mcp) (fboundp 'mcp-hub-get-servers)
                  (seq-find (lambda (s)
                              (and (equal (plist-get s :name) server)
                                   (eq (plist-get s :status) 'connected)))
                            (mcp-hub-get-servers)))
             '(:box (:line-width -1) :inherit success)
           '(:box (:line-width -1) :inherit shadow)))
     prepend))
  "Font-lock keyword list for `@ref:'/`@ref{}'/`@file:'/`@agent:'/`@mcp:' mentions.")

(defun mevedel-agent-capf ()
  "Completion-at-point function for @agent:name mentions.
Completes against registered agents in `mevedel-agent--registry',
using each agent's description as the candidate annotation."
  (when (and (boundp 'mevedel-agent--registry) mevedel-agent--registry)
    (save-excursion
      (let ((orig-point (point)))
        (when (re-search-backward "@agent:" (line-beginning-position) t)
          (let* ((prefix-start (point))
                 (name-start (+ prefix-start 7))
                 (name-end (progn
                             (goto-char orig-point)
                             (skip-chars-forward "[:alnum:]_-")
                             (point))))
            (when (<= orig-point name-end)
              (let ((candidates
                     (mapcar
                      (lambda (entry)
                        (let* ((agent (cdr entry))
                               (name (mevedel-agent-name agent))
                               (desc (or (mevedel-agent-description agent) "")))
                          (propertize name 'mevedel-agent-desc desc)))
                      mevedel-agent--registry)))
                (list name-start name-end candidates
                      :exclusive 'no
                      :annotation-function
                      (lambda (cand)
                        (let ((desc (get-text-property 0 'mevedel-agent-desc cand)))
                          (if (and desc (not (string-empty-p desc)))
                              (format " %s"
                                      (truncate-string-to-width
                                       (string-trim
                                        (replace-regexp-in-string "[\n\r]+" " " desc))
                                       60 nil nil "..."))
                            ""))))))))))))

(defun mevedel-mention-capf ()
  "Completion-at-point for the leading `@' of mention prefixes.
Offers `@ref:', `@ref:{}', `@file:', `@agent:', and `@mcp:' as
candidates when point sits immediately after `@' possibly followed by
alphabetic characters.  Once the prefix is complete, the type-specific
capfs (`mevedel-ref-capf' etc.) take over.  For `@ref:{}' the
exit-function positions point between the braces."
  (save-excursion
    (let ((orig-point (point)))
      (skip-chars-backward "[:alpha:]")
      (when (and (> (point) (point-min))
                 (eq (char-before) ?@))
        (let ((start (1- (point)))
              (end orig-point))
          (list start end
                '("@ref:" "@ref:{}" "@file:" "@agent:" "@mcp:")
                :exclusive 'no
                :annotation-function
                (lambda (cand)
                  (pcase cand
                    ("@ref:"   " [reference by ID]")
                    ("@ref:{}" " [references by tag query]")
                    ("@file:"  " [file path or directory listing]")
                    ("@agent:" " [delegate to sub-agent]")
                    ("@mcp:"   " [MCP server resource]")
                    (_ "")))
                :exit-function
                (lambda (str status)
                  (when (and (memq status '(finished sole exact))
                             (equal str "@ref:{}"))
                    (backward-char 1)))))))))

(defun mevedel-mcp-capf ()
  "Completion-at-point function for @mcp:server:uri mentions.
At `@mcp:' completes against configured server names.  At
`@mcp:server:' completes against that server's listed resource URIs.
Requires mcp.el to be loaded; returns nil otherwise."
  (when (and (featurep 'mcp) (fboundp 'mcp-hub-get-servers))
    (save-excursion
      (let ((orig-point (point)))
        (when (re-search-backward "@mcp:" (line-beginning-position) t)
          (let* ((prefix-start (point))
                 (after-prefix (+ prefix-start 5))
                 (tail (buffer-substring-no-properties after-prefix orig-point))
                 (servers (mcp-hub-get-servers)))
            (cond
             ;; No second colon yet -- complete server names.
             ((not (string-match-p ":" tail))
              (let* ((name-end (progn
                                 (goto-char orig-point)
                                 (skip-chars-forward "^: \t\n")
                                 (point)))
                     (candidates
                      (mapcar
                       (lambda (s)
                         (let ((name (plist-get s :name))
                               (status (plist-get s :status)))
                           (propertize name 'mevedel-mcp-status status)))
                       servers)))
                (list after-prefix name-end candidates
                      :exclusive 'no
                      :annotation-function
                      (lambda (cand)
                        (let ((status (get-text-property 0 'mevedel-mcp-status cand)))
                          (format " [%s]" (or status "?")))))))
             ;; Second colon present -- complete resource URIs for that server.
             (t
              (let* ((colon-pos (string-match ":" tail))
                     (server-name (substring tail 0 colon-pos))
                     (uri-start (+ after-prefix colon-pos 1))
                     (uri-end (progn
                                (goto-char orig-point)
                                (skip-chars-forward "^ \t\n")
                                (point)))
                     (server-info (seq-find
                                   (lambda (s)
                                     (equal (plist-get s :name) server-name))
                                   servers))
                     (resources (and server-info (plist-get server-info :resources)))
                     (resource-list (if (vectorp resources)
                                        (append resources nil)
                                      resources))
                     (candidates
                      (mapcar
                       (lambda (r)
                         (let ((uri (plist-get r :uri))
                               (name (plist-get r :name))
                               (desc (plist-get r :description)))
                           (propertize uri
                                       'mevedel-mcp-name name
                                       'mevedel-mcp-desc desc)))
                       resource-list)))
                (when candidates
                  (list uri-start uri-end candidates
                        :exclusive 'no
                        :annotation-function
                        (lambda (cand)
                          (let ((name (get-text-property 0 'mevedel-mcp-name cand))
                                (desc (get-text-property 0 'mevedel-mcp-desc cand)))
                            (cond
                             ((and desc (not (string-empty-p desc)))
                              (format " %s"
                                      (truncate-string-to-width
                                       (string-trim
                                        (replace-regexp-in-string
                                         "[\n\r]+" " " desc))
                                       60 nil nil "...")))
                             ((and name (not (string-empty-p name)))
                              (format " %s" name))
                             (t "")))))))))))))))

(defun mevedel-mentions-install ()
  "Install font-lock and completion support for mevedel mentions.
Adds font-lock keywords for @ref/@file/@agent/@mcp and pushes the
mention-specific capfs onto the buffer-local
`completion-at-point-functions'.  `mevedel-mention-capf' handles the
bare `@' prefix; the other capfs fire once the prefix is complete."
  (dolist (kw mevedel-mentions--font-lock-keywords)
    (font-lock-add-keywords nil (list kw) t))
  (add-hook 'completion-at-point-functions #'mevedel-mention-capf nil t)
  (add-hook 'completion-at-point-functions #'mevedel-ref-capf nil t)
  (add-hook 'completion-at-point-functions #'mevedel-file-capf nil t)
  (add-hook 'completion-at-point-functions #'mevedel-agent-capf nil t)
  (add-hook 'completion-at-point-functions #'mevedel-mcp-capf nil t))

(provide 'mevedel-mentions)
;;; mevedel-mentions.el ends here
