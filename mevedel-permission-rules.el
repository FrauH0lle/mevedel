;;; mevedel-permission-rules.el -- Permission rule matching -*- lexical-binding: t -*-

;;; Commentary:

;; Owns permission rule configuration, parsing, matching, precedence buckets,
;; protected-path policy, and exact resource-grant construction.

;;; Code:

(require 'cl-lib)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(autoload 'mevedel-execution-target-expand-path "mevedel-execution-target")

;; `mevedel-tool-registry'
(declare-function mevedel-tool-ensure "mevedel-tool-registry" (name))
(declare-function mevedel-tool-get
                  "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-get-domain "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-paths "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-pattern "mevedel-tool-registry" (cl-x) t)
(autoload 'mevedel-tool-ensure "mevedel-tool-registry")
(autoload 'mevedel-tool-get "mevedel-tool-registry")
(autoload 'mevedel-tool-get-domain "mevedel-tool-registry")
(autoload 'mevedel-tool-get-name "mevedel-tool-registry")
(autoload 'mevedel-tool-get-path "mevedel-tool-registry")
(autoload 'mevedel-tool-get-paths "mevedel-tool-registry")
(autoload 'mevedel-tool-get-pattern "mevedel-tool-registry")


;;
;;; Customization

(defcustom mevedel-permission-rules
  nil
  "Permission rules for tools.

Each entry is a list:
  (TOOL-NAME &key SPECIFIER VALUE :network BOOLEAN
                   :file-system GRANTS
                   :sandbox-permissions LEVEL
                   :action ACTION)

TOOL-NAME is a string matching a tool name (e.g., \"Read\", \"ApplyPatch\"),
or \"*\" to match all tools.

SPECIFIER is optional and selects what aspect of the invocation the
rule matches against.  At most one specifier is allowed per rule:

  :path    GLOB  - filesystem path (supports *, **, ?, ~)
                   Used by Read, ApplyPatch, Glob, Grep, Bash
                   when it resolves a bare path.
  :pattern GLOB  - command or expression string (supports *, plus
                   Bash-style PREFIX:*).  Used by Bash and by qualified
                   full-escalation Eval rules.
  :domain  GLOB  - host name (supports *)
                   Used by WebFetch and WebSearch.
  :name    GLOB  - match name (supports *)
                   Used by Agent (task_name).

Rules without a specifier match the tool regardless of context.

The optional `:network t' qualifier records reusable network authority for
the matching Bash command or Eval expression.  It also authorizes the
operation; an otherwise identical rule without `:network' leaves network
isolated.

The optional `:file-system' qualifier records exact child-process grants as
`((:path ABSOLUTE-PATH :access ACCESS) ...)', where ACCESS is `read' or
`write'.  A matching direct resource grant must also exist before a path is
reopened.

The optional :sandbox-permissions qualifier currently accepts
`require-escalated'.  Such rules participate only in full execution
escalation decisions; they do not grant ordinary tool permission.  A
qualified `allow' deliberately authorizes matching code to run directly
as the Emacs user, without filesystem, network, or process confinement.

ACTION is one of: `allow', `deny', or `ask'.

Precedence within matching rules:
  1. Specifier-carrying rules > unqualified (generic) rules.
  2. Within each group: deny > ask > allow.

Example:
  ((\"Read\" :action allow)
   (\"ApplyPatch\" :path \"~/projects/**\" :action allow)
   (\"ApplyPatch\" :path \"~/.ssh/**\" :action deny)
   (\"Bash\" :pattern \"ls *\" :action allow)
   (\"Bash\" :pattern \"git log:*\" :action allow)
   (\"Bash\" :pattern \"npx test*\" :network t :action allow)
   (\"Bash\" :pattern \"make report\"
           :file-system ((:path \"/srv/report\" :access write))
           :action allow)
   (\"Bash\" :pattern \"rm *\" :action deny)
   (\"Bash\" :pattern \"curl https://example.com/*\"
           :sandbox-permissions require-escalated :action allow)
   (\"Eval\" :pattern \"(my-trusted-batch-job*)\"
           :sandbox-permissions require-escalated :action allow)
   (\"WebFetch\" :domain \"*.example.com\" :action allow)
   (\"Agent\" :name \"explorer\" :action allow))"
  :type '(repeat sexp)
  :group 'mevedel)

(defcustom mevedel-protected-paths
  '(("**/.git/**" . read-only)
    ("~/.ssh/**" . inaccessible)
    ("~/.gnupg/**" . inaccessible)
    ("~/.aws/**" . inaccessible)
    ("~/.azure/**" . inaccessible)
    ("~/.config/gcloud/**" . inaccessible)
    ("~/.kube/**" . inaccessible))
  "Protected path globs and their child-confinement access modes.

Even `full-auto' mode prompts when a matching path lacks an exact resource
grant.  `read-only' keeps matched content visible but immutable;
`inaccessible' hides it.  This alist is also compiled into the Bubblewrap
  profile for Bash and batch Eval."
  :type '(alist :key-type string
                :value-type
                (choice (const inaccessible) (const read-only)))
  :group 'mevedel)


;;
;;; allowed-tools parsing

(defun mevedel-permission--tool-specifier-key (tool-name)
  "Return the specifier keyword for TOOL-NAME, or nil if absent.

Looks the tool up in the registry and returns `:pattern', `:domain',
`:name', or `:path' based on which `get-*' slot is populated.  Returns
nil when TOOL-NAME is unknown or declares no getter.

Note: specifier keyword semantics are per-tool, not per-keyword.
Both `Skill' and `Agent' use `:name', but `Skill :name' matches the
skill name (invocation identifier) while `Agent :name' matches the
task_name.  The keyword is a syntactic slot; the matching
semantics are owned by the tool's `get-name' getter.  Authors of
permission rules should consult each tool's documentation rather
than assume cross-tool uniformity for the same keyword."
  (when-let* ((tool (mevedel-tool-get tool-name)))
    (cond ((mevedel-tool-get-pattern tool) :pattern)
          ((mevedel-tool-get-domain  tool) :domain)
          ((mevedel-tool-get-name    tool) :name)
          ((or (mevedel-tool-get-path tool)
               (mevedel-tool-get-paths tool)) :path))))

(defun mevedel-permission-rules-parse (entry)
  "Parse ENTRY (an `allowed-tools' string) into a rule.

Returns a rule plist of the form
\\=`(TOOL-NAME &key SPECIFIER VALUE :action allow)' suitable for
`mevedel-permission-rules', or signals `user-error' on bad input.

Recognised forms:

- `\"Read\"'              bare tool name
- `\"ApplyPatch(src/**)\"' qualified by path
- `\"Bash(git status)\"'  qualified by exact pattern
- `\"Bash(git status *)\"' qualified by glob pattern
- `\"WebFetch(example.com)\"' qualified by domain
- `\"Agent(spec_review)\"' qualified by child task name

Specifier inference is registry-driven via
`mevedel-permission--tool-specifier-key', so a new tool that ships
with `:get-domain' is automatically qualifiable in skill frontmatter
without editing the permission layer.

Failure modes:

- malformed syntax (no balanced parens, unrecognized shape) ->
  `user-error \"Malformed allowed-tools entry: ENTRY\"'
- unknown tool name -> `user-error'
- qualifier on a tool with no specifier slot (e.g. `\"Ask(foo)\"') ->
  `user-error'"
  (unless (stringp entry)
    (user-error "Malformed allowed-tools entry: %S (must be a string)" entry))
  (let ((case-fold-search nil))
    (cond
     ;; Bare name: ^Tool$
     ((string-match "\\`\\([A-Za-z][A-Za-z0-9]*\\)\\'" entry)
      (let ((tool-name (match-string 1 entry)))
        (unless (mevedel-tool-ensure tool-name)
          (user-error "Unknown tool in allowed-tools: %s" tool-name))
        (list tool-name :action 'allow)))
     ;; Qualified: ^Tool(VALUE)$
     ((string-match
       "\\`\\([A-Za-z][A-Za-z0-9]*\\)(\\(.*\\))\\'" entry)
      (let* ((tool-name (match-string 1 entry))
             (raw-value (match-string 2 entry))
             (value raw-value))
        (unless (mevedel-tool-ensure tool-name)
          (user-error "Unknown tool in allowed-tools: %s" tool-name))
        (let ((spec-key (mevedel-permission--tool-specifier-key tool-name)))
          (unless spec-key
            (user-error "Tool %s does not support qualifiers" tool-name))
          (list tool-name spec-key value :action 'allow))))
     (t
     (user-error "Malformed allowed-tools entry: %s" entry)))))


;;
;;; Rule matching

(defun mevedel-permission-rules-match-path-p (path pattern &optional target)
  "Check if PATH matches glob PATTERN.

PATTERN supports:
  *   - matches any sequence of characters except /
  **  - matches any sequence of characters including /
  ?   - matches any single character
  ~   - expanded to home directory at pattern start

When TARGET is non-nil, expand a leading `~' from that target's probed
environment.  Absolute patterns remain in the client path domain.
Returns non-nil if PATH matches PATTERN."
  (when (and path pattern)
    (let* ((expanded
            (cond
             ((and target (string-prefix-p "~" pattern))
              (mevedel-execution-target-expand-path target pattern))
             ((or (string-prefix-p "~" pattern)
                  (file-name-absolute-p pattern))
              (expand-file-name pattern))
             (t pattern)))
           (expanded-path (expand-file-name path))
           (directory-glob-root
            (when (string-suffix-p "/**" expanded)
              (substring expanded 0 -3)))
           (match-pattern (or directory-glob-root expanded))
           (i 0)
           (len (length match-pattern))
           (parts (list "\\`")))
      (while (< i len)
        (let ((ch (aref match-pattern i)))
          (cond
           ;; ** globstar - matches across directories
           ((and (eq ch ?*)
                 (< (1+ i) len)
                 (eq (aref match-pattern (1+ i)) ?*))
            (push ".*" parts)
            (setq i (+ i 2)))
           ;; * - matches within a single directory
           ((eq ch ?*)
            (push "[^/]*" parts)
            (setq i (1+ i)))
           ;; ? - matches single character
           ((eq ch ??)
            (push "." parts)
            (setq i (1+ i)))
           ;; Literal character
           (t
            (push (regexp-quote (char-to-string ch)) parts)
            (setq i (1+ i))))))
      (push (if directory-glob-root "\\(?:/.*\\)?\\'" "\\'") parts)
      (string-match-p (apply #'concat (nreverse parts)) expanded-path))))

(defconst mevedel-permission--specifier-keys
  '(:path :pattern :domain :name)
  "Keys recognised as rule specifiers.
A rule may carry at most one of these.  The first match wins.")

(defun mevedel-permission--rule-specifier (rule)
  "Return (KEY . VALUE) for RULE's specifier, or (nil . nil) if unqualified."
  (let ((plist (cdr rule))
        result)
    (cl-loop for key in mevedel-permission--specifier-keys
             when (plist-member plist key)
             do (setq result (cons key (plist-get plist key)))
             and return nil)
    (or result (cons nil nil))))

(defun mevedel-permission--match-specifier (kind pattern value)
  "Return non-nil if VALUE matches PATTERN under specifier KIND."
  (when (and pattern value)
    (pcase kind
      (:path (mevedel-permission-rules-match-path-p value pattern))
      (:pattern
       (if (string-suffix-p ":*" pattern)
           (let ((prefix (substring pattern 0 -2)))
             (or (string= value prefix)
                 (string-prefix-p (concat prefix " ") value)))
         (string-match-p (wildcard-to-regexp pattern) value)))
      ((or :domain :name)
       (string-match-p (wildcard-to-regexp pattern) value)))))

(cl-defun mevedel-permission-rules-find
    (rules tool-name &key path pattern domain name)
  "Find all matching rules in RULES for TOOL-NAME under the given values.

RULES is a list in the format of `mevedel-permission-rules'.  Rule
matches are determined by the rule's specifier (one of `:path',
`:pattern', `:domain', `:name') against PATH, PATTERN, DOMAIN, or NAME.
Unqualified rules match unconditionally.  Return a list of
matching rules in order (later entries = higher priority)."
  (let ((matches nil)
        (values `((:path    . ,path)
                  (:pattern . ,pattern)
                  (:domain  . ,domain)
                  (:name    . ,name))))
    (dolist (rule rules)
      (let ((rule-tool (car rule)))
        (when (and (not (plist-member (cdr rule) :sandbox-permissions))
                   (or (equal rule-tool "*")
                       (equal rule-tool tool-name)))
          (let* ((spec (mevedel-permission--rule-specifier rule))
                 (kind (car spec))
                 (rule-value (cdr spec)))
            (cond
             ;; No specifier: matches unconditionally
             ((null kind)
              (push rule matches))
             ;; Specifier present: compare against the corresponding value
             ((mevedel-permission--match-specifier
               kind rule-value (alist-get kind values))
              (push rule matches)))))))
    (nreverse matches)))

(cl-defun mevedel-permission-rules-action
    (rules tool-name &key path pattern domain name)
  "Determine the effective action from RULES for TOOL-NAME and specifiers.

Rules that carry a specifier (any of `:path', `:pattern', `:domain',
`:name') match PATH, PATTERN, DOMAIN, or NAME and take precedence over
unqualified rules.  Within each group, deny > ask > allow.  Return
`allow', `deny', `ask', or nil if no rules match."
  (let ((matching (mevedel-permission-rules-find
                   rules tool-name
                   :path path :pattern pattern
                   :domain domain :name name))
        (spec-deny nil) (spec-ask nil) (spec-allow nil)
        (gen-deny nil) (gen-ask nil) (gen-allow nil))
    (dolist (rule matching)
      (let ((has-spec (car (mevedel-permission--rule-specifier rule))))
        (pcase (plist-get (cdr rule) :action)
          ('deny (if has-spec (setq spec-deny t) (setq gen-deny t)))
          ('ask (if has-spec (setq spec-ask t) (setq gen-ask t)))
          ('allow (if has-spec (setq spec-allow t) (setq gen-allow t))))))
    (cond
     ;; Specifier-carrying rules take precedence
     (spec-deny 'deny)
     (spec-ask 'ask)
     (spec-allow 'allow)
     ;; Generic rules
     (gen-deny 'deny)
     (gen-ask 'ask)
     (gen-allow 'allow)
     (t nil))))


;;
;;; Protected paths

(defun mevedel-permission-protected-path-policy ()
  "Return validated entries from `mevedel-protected-paths'."
  (dolist (entry mevedel-protected-paths)
    (unless (and (consp entry)
                 (stringp (car entry))
                 (memq (cdr entry) '(read-only inaccessible)))
      (error "Invalid protected path entry: %S" entry)))
  mevedel-protected-paths)

(defun mevedel-permission-rules-path-protected-p (path &optional target)
  "Check if PATH matches any pattern in `mevedel-protected-paths'.

TARGET supplies the path domain for target-home patterns.
Returns non-nil if the path is protected."
  (when path
    (let ((expanded (expand-file-name path)))
      (cl-loop for (pattern . _mode) in
               (mevedel-permission-protected-path-policy)
               thereis
               (mevedel-permission-rules-match-path-p
                expanded pattern target)))))

(defun mevedel-permission--path-in-workspace-p (path workspace-root)
  "Return non-nil when PATH is WORKSPACE-ROOT or is contained by it."
  (when (and path workspace-root)
    (let ((abs-path (expand-file-name path))
          (abs-root (expand-file-name workspace-root)))
      (or (string= (directory-file-name abs-path)
                   (directory-file-name abs-root))
          (string-prefix-p (file-name-as-directory abs-root)
                           abs-path)))))

(defun mevedel-permission-rules-path-in-allowed-roots-p (path roots)
  "Return non-nil when PATH is contained by any root in ROOTS."
  (when path
    (cl-loop for root in roots
             thereis (mevedel-permission--path-in-workspace-p path root))))

(defun mevedel-permission-rules-path-in-exact-allowed-paths-p (path allowed-paths)
  "Return non-nil when PATH exactly matches one of ALLOWED-PATHS."
  (when path
    (let ((expanded (expand-file-name path)))
      (cl-loop for allowed in allowed-paths
               thereis (string= expanded (expand-file-name allowed))))))

(defun mevedel-permission-rules-resource-granted-p (path access grants)
  "Return non-nil when GRANTS authorize ACCESS to exact PATH."
  (when (and path (memq access '(read write)))
    (let ((expanded (expand-file-name path)))
      (cl-loop for grant in grants
               when (proper-list-p grant)
               thereis
               (and (stringp (plist-get grant :path))
                    (string= expanded
                             (expand-file-name (plist-get grant :path)))
                    (or (eq (plist-get grant :access) 'write)
                        (eq (plist-get grant :access) access)))))))


;;
;;; Decision chain -- bucket-aware

(defun mevedel-permission-rules-collect-buckets
    (invocation-rules request-rules session-rules persistent-rules)
  "Return permission-rule buckets from all rule layers.

INVOCATION-RULES, REQUEST-RULES, SESSION-RULES, and PERSISTENT-RULES are
the dynamic rule layers.

Buckets are listed innermost-first; pass 2 (allow/ask) walks them
in order and returns the first non-nil decision.  Pass 1 (deny)
is order-insensitive but reuses the same alist."
  `((:invocation . ,invocation-rules)
    (:request    . ,request-rules)
    (:session    . ,session-rules)
    (:persistent . ,persistent-rules)
    (:defcustom  . ,mevedel-permission-rules)))

(defun mevedel-permission--bucket-action
    (bucket-rules tool-name path pattern domain name)
  "Resolve BUCKET-RULES action for TOOL-NAME, PATH, PATTERN, DOMAIN, and NAME."
  (mevedel-permission-rules-action
   bucket-rules tool-name
   :path path :pattern pattern :domain domain :name name))

(defun mevedel-permission-rules-first-deny-bucket
    (buckets tool-name path pattern domain name)
  "Return first BUCKETS key denying TOOL-NAME for PATH, PATTERN, DOMAIN, or NAME."
  (cl-loop for (key . rules) in buckets
           when (eq (mevedel-permission--bucket-action
                     rules tool-name path pattern domain name)
                    'deny)
           return key))

(defun mevedel-permission--any-deny
    (buckets tool-name path pattern domain name)
  "Return non-nil if BUCKETS deny TOOL-NAME for PATH, PATTERN, DOMAIN, or NAME.
BUCKETS is the alist from `mevedel-permission-rules-collect-buckets'."
  (not (null (mevedel-permission-rules-first-deny-bucket
              buckets tool-name path pattern domain name))))

(defun mevedel-permission-rules-first-action-with-bucket
    (buckets tool-name path pattern domain name)
  "Walk BUCKETS for TOOL-NAME and return (ACTION . BUCKET).

PATH, PATTERN, DOMAIN, and NAME are the specifier values."
  (cl-loop for (key . rules) in buckets
           for action = (mevedel-permission--bucket-action
                         rules tool-name path pattern domain name)
           when action return (cons action key)))

(defun mevedel-permission--first-non-nil-action
    (buckets tool-name path pattern domain name)
  "Walk BUCKETS for TOOL-NAME and return first non-nil bucket action.

PATH, PATTERN, DOMAIN, and NAME are the specifier values."
  (car (mevedel-permission-rules-first-action-with-bucket
        buckets tool-name path pattern domain name)))

(defun mevedel-permission-rules-bucket-decision
    (buckets tool-name path pattern domain name)
  "Resolve BUCKETS with absolute deny precedence, then inner-first authority."
  (if (mevedel-permission--any-deny
       buckets tool-name path pattern domain name)
      'deny
    (mevedel-permission--first-non-nil-action
     buckets tool-name path pattern domain name)))

(defun mevedel-permission-rules-qualified-buckets (buckets qualifier value)
  "Return direct authority in BUCKETS qualified by QUALIFIER and VALUE.
Delegated buckets retain qualified denies but cannot grant qualified
authority.  QUALIFIER is removed from the returned rules."
  (mapcar
   (lambda (entry)
     (let ((bucket (car entry)))
       (cons
        bucket
        (cl-loop
         for rule in (cdr entry)
         when (and (eq (plist-get (cdr rule) qualifier) value)
                   (or (memq bucket '(:session :persistent :defcustom))
                       (eq 'deny (plist-get (cdr rule) :action))))
         collect
         (cons
          (car rule)
          (cl-loop for (key item) on (cdr rule) by #'cddr
                   unless (eq key qualifier)
                   append (list key item)))))))
   buckets))

(defun mevedel-permission-rules-execution-level-decision
    (buckets tool-name level pattern)
  "Resolve direct user authority for TOOL-NAME, LEVEL, and PATTERN.
Qualified denies in any bucket remain final.  Only session, persistent, and
defcustom buckets may otherwise authorize or explicitly ask for LEVEL."
  (mevedel-permission-rules-bucket-decision
   (mevedel-permission-rules-qualified-buckets
    buckets :sandbox-permissions level)
   tool-name nil pattern nil nil))

(defun mevedel-permission-rules-network-decision
    (buckets tool-name pattern)
  "Resolve direct reusable network authority for TOOL-NAME and PATTERN."
  (mevedel-permission-rules-bucket-decision
   (mevedel-permission-rules-qualified-buckets buckets :network t)
   tool-name nil pattern nil nil))


;;
;;; Session rule storage

(defun mevedel-permission-rules-resource-grant (path access)
  "Return normalized exact PATH ACCESS resource grant."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Invalid resource path: %S" path))
  (unless (memq access '(read write))
    (error "Invalid resource access: %s" access))
  (list :path (expand-file-name path) :access access))

(defun mevedel-permission-rules-merge-resource-grant (grants path access)
  "Return GRANTS with exact PATH promoted to ACCESS."
  (let ((grant (mevedel-permission-rules-resource-grant path access)))
    (if (mevedel-permission-rules-resource-granted-p path access grants)
        grants
      (append
       (if (eq access 'write)
           (cl-remove
            (expand-file-name path) grants
            :key (lambda (item)
                   (and (stringp (plist-get item :path))
                        (expand-file-name (plist-get item :path))))
            :test #'string=)
         grants)
       (list grant)))))

(cl-defun mevedel-permission-rules-build-rule
    (tool-name action spec-key spec-value
               &key network file-system sandbox-permissions)
  "Build a permission rule list from the given components.

TOOL-NAME is the tool name string or \"*\".  ACTION is `allow', `deny',
or `ask'.  SPEC-KEY is one of `:path', `:pattern', `:domain', `:name',
or nil for an unqualified rule.  SPEC-VALUE is the glob associated with
SPEC-KEY (ignored when SPEC-KEY is nil).  NETWORK and FILE-SYSTEM record a
matching additive execution profile.  SANDBOX-PERMISSIONS optionally qualifies
the already requested child-execution level."
  (append
   (list tool-name)
   (and spec-key spec-value (list spec-key spec-value))
   (and network (list :network t))
   (and file-system (list :file-system file-system))
   (and sandbox-permissions
        (list :sandbox-permissions sandbox-permissions))
   (list :action action)))

(provide 'mevedel-permission-rules)
;;; mevedel-permission-rules.el ends here
