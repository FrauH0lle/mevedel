;;; mevedel-specialist-nudges.el -- Specialist tool prompting policy -*- lexical-binding: t -*-

;;; Commentary:

;; Owns eligibility, throttling, and model-visible guidance that steers
;; generic code navigation toward available specialist tools.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-deferred-injected
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-deferred-set
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-set-specialist-nudge-state
                  "mevedel-agents" (invocation state))
(declare-function mevedel-agent-invocation-specialist-nudge-state
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-turn-count
                  "mevedel-agents" (cl-x) t)

;; `mevedel-reminders'
(declare-function mevedel-reminders-specialist-capabilities
                  "mevedel-reminders" (session))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)


;;
;;; Specialist tool nudges

(defconst mevedel-specialist-nudges--specialist-nudge-max-per-family 2
  "Maximum number of times to nudge each specialist family per context.")

(defconst mevedel-specialist-nudges--code-file-extensions
  '("c" "cc" "clj" "cljs" "cpp" "cs" "css" "dart" "el" "ex"
    "exs" "go" "h" "hpp" "hs" "java" "js" "jsx" "kt" "kts"
    "lua" "m" "mm" "php" "py" "rb" "rs" "scala" "scm" "sh"
    "swift" "ts" "tsx" "vue")
  "File extensions treated as code for specialist nudge heuristics.")

(defconst mevedel-specialist-nudges--code-rg-types
  '("c" "clojure" "cpp" "csharp" "css" "dart" "elisp" "go"
    "haskell" "java" "js" "jsx" "kotlin" "lua" "objc" "php"
    "py" "python" "rb" "ruby" "rust" "scala" "scheme" "sh"
    "swift" "ts" "tsx")
  "Ripgrep type names treated as code for specialist nudge heuristics.")

(defconst mevedel-specialist-nudges--structural-code-patterns
  '("class" "defclass" "defcustom" "defmacro" "defmethod" "defstruct"
    "defun" "enum" "function" "if" "import" "interface" "lambda"
    "let" "module" "namespace" "require" "struct" "switch" "try"
    "while")
  "Plain code patterns that usually indicate structural search.")

(defun mevedel-specialist-nudges--ctx-deferred-set (ctx)
  "Return CTX's deferred-set slot, or nil."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-deferred-set ctx))
   ((mevedel-session-p ctx)
    (mevedel-session-deferred-set ctx))))

(defun mevedel-specialist-nudges--ctx-deferred-injected (ctx)
  "Return CTX's deferred-injected slot, or nil."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-deferred-injected ctx))
   ((mevedel-session-p ctx)
    (mevedel-session-deferred-injected ctx))))

(defun mevedel-specialist-nudges--ctx-nudge-state (ctx)
  "Return CTX's specialist nudge state."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-specialist-nudge-state ctx))
   ((mevedel-session-p ctx)
    (mevedel-session-specialist-nudge-state ctx))))

(defun mevedel-specialist-nudges--set-ctx-nudge-state (ctx state)
  "Set CTX's specialist nudge STATE."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (mevedel-agent-invocation-set-specialist-nudge-state ctx state))
   ((mevedel-session-p ctx)
    (setf (mevedel-session-specialist-nudge-state ctx) state))))

(defun mevedel-specialist-nudges--ctx-turn-count (ctx session)
  "Return CTX's turn count, falling back to SESSION."
  (cond
   ((and (fboundp 'mevedel-agent-invocation-p)
         (mevedel-agent-invocation-p ctx))
    (or (mevedel-agent-invocation-turn-count ctx) 0))
   ((mevedel-session-p ctx) (or (mevedel-session-turn-count ctx) 0))
   ((mevedel-session-p session) (or (mevedel-session-turn-count session) 0))
   (t 0)))

(defun mevedel-specialist-nudges--tool-name-present-p (ctx names)
  "Return non-nil when CTX has any deferred or injected tool in NAMES."
  (or (cl-some (lambda (entry) (member (cadr (car entry)) names))
               (mevedel-specialist-nudges--ctx-deferred-set ctx))
      (cl-some (lambda (entry) (member (car entry) names))
               (mevedel-specialist-nudges--ctx-deferred-injected ctx))))

(defun mevedel-specialist-nudges--nudge-context (context)
  "Return CONTEXT owner whose nudge throttle should be updated."
  (or (plist-get context :invocation)
      (plist-get context :session)))

(defun mevedel-specialist-nudges--nudge-allowed-p (context family)
  "Return non-nil when FAMILY may nudge for CONTEXT, recording use."
  (if-let* ((ctx (mevedel-specialist-nudges--nudge-context context)))
      (let* ((session (plist-get context :session))
             (turn (mevedel-specialist-nudges--ctx-turn-count ctx session))
             (state (copy-sequence (mevedel-specialist-nudges--ctx-nudge-state ctx)))
             (entry (plist-get state family))
             (count (or (plist-get entry :count) 0))
             (last-turn (plist-get entry :turn)))
        (when (and (< count mevedel-specialist-nudges--specialist-nudge-max-per-family)
                   (not (equal last-turn turn)))
          (setq state (plist-put state family (list :count (1+ count) :turn turn)))
          (mevedel-specialist-nudges--set-ctx-nudge-state ctx state)
          t))
    t))

(defun mevedel-specialist-nudges--code-path-p (path)
  "Return non-nil when PATH names a code file."
  (and (stringp path)
       (member (downcase (or (file-name-extension path) ""))
               mevedel-specialist-nudges--code-file-extensions)))

(defun mevedel-specialist-nudges--code-glob-p (glob)
  "Return non-nil when GLOB appears scoped to code files."
  (and (stringp glob)
       (cl-some (lambda (ext)
                  (string-match-p
                   (concat "\\." (regexp-quote ext) "\\_>") glob))
                mevedel-specialist-nudges--code-file-extensions)))

(defun mevedel-specialist-nudges--grep-result-code-p (result)
  "Return non-nil when RESULT has code-file paths."
  (and (stringp result)
       (cl-some (lambda (line)
                  (let ((path (car (split-string line ":" t))))
                    (mevedel-specialist-nudges--code-path-p path)))
                (split-string result "\n" t))))

(defun mevedel-specialist-nudges--grep-code-target-p (args result)
  "Return non-nil when Grep ARGS/RESULT point at code files."
  (or (member (downcase (or (plist-get args :type) ""))
              mevedel-specialist-nudges--code-rg-types)
      (mevedel-specialist-nudges--code-glob-p (plist-get args :glob))
      (mevedel-specialist-nudges--code-path-p (plist-get args :path))
      (mevedel-specialist-nudges--grep-result-code-p result)))

(defun mevedel-specialist-nudges--identifier-like-pattern-p (pattern)
  "Return non-nil when PATTERN matches a specific code identifier."
  (and (stringp pattern)
       (string-match-p "\\`[[:alpha:]_][[:alnum:]_-]*\\'" pattern)
       (or (string-match-p "[-_]" pattern)
           (let ((case-fold-search nil))
             (string-match-p "[[:lower:]][[:upper:]]\\|[[:upper:]][[:lower:]]"
                             pattern)))))

(defun mevedel-specialist-nudges--structural-code-pattern-p (pattern)
  "Return non-nil when PATTERN is a structural code search."
  (and (stringp pattern)
       (member (downcase pattern)
               mevedel-specialist-nudges--structural-code-patterns)))

(defun mevedel-specialist-nudges--grep-comment-result-p (result)
  "Return non-nil when Grep RESULT appears to contain only comment hits."
  (and (stringp result)
       (let ((lines (split-string result "\n" t)))
         (and lines
              (cl-every
               (lambda (line)
                 (let ((text (if (string-match
                                  "\\`[^:\n]+:[0-9]+:\\(?:[0-9]+:\\)?\\(.*\\)\\'"
                                  line)
                                 (match-string 1 line)
                               line)))
                   (string-match-p
                    "\\`[[:space:]]*\\(?:;;\\|//\\|#\\|/\\*\\|\\*\\|;\\)"
                    text)))
               lines)))))

(defun mevedel-specialist-nudges--read-text-range-p (args)
  "Return non-nil when Read ARGS request a non-default text range."
  (let ((offset (plist-get args :offset))
        (limit (plist-get args :limit)))
    (or (and offset (not (equal offset 0)))
        (and limit (not (member limit '(0 2000)))))))

(defun mevedel-specialist-nudges--read-media-range-p (args)
  "Return non-nil when Read ARGS request explicit media handling."
  (or (let ((pages (plist-get args :pages)))
        (and pages (not (equal pages ""))))
      (cl-some (lambda (key)
                 (let ((value (plist-get args key)))
                   (and value (not (equal value 0)))))
               '(:max_width :max_height :max_tokens))))

(defun mevedel-specialist-nudges--read-exact-range-p (args)
  "Return non-nil when Read ARGS request an exact text/media range."
  (or (mevedel-specialist-nudges--read-text-range-p args)
      (mevedel-specialist-nudges--read-media-range-p args)))

(defun mevedel-specialist-nudges--specialist-capabilities (session)
  "Return specialist capabilities for SESSION, or nil if unavailable."
  (when session
    (require 'mevedel-reminders)
    (mevedel-reminders-specialist-capabilities session)))

(defun mevedel-specialist-nudges--family-applicable-p (context caps family names)
  "Return non-nil when CAPS or CONTEXT enables FAMILY for NAMES."
  (or (plist-get caps family)
      (mevedel-specialist-nudges--tool-name-present-p
       (mevedel-specialist-nudges--nudge-context context) names)))

(defun mevedel-specialist-nudges--specialist-load-text (context names query)
  "Return a ToolSearch hint for NAMES and QUERY in CONTEXT."
  (let ((ctx (mevedel-specialist-nudges--nudge-context context)))
    (when (cl-some (lambda (entry) (member (cadr (car entry)) names))
                   (mevedel-specialist-nudges--ctx-deferred-set ctx))
      (format " If the tool is not callable, use ToolSearch(query=\"%s\", load=true); loaded tools are available now for your next tool call."
              query))))

(defun mevedel-specialist-nudges--grep-specialist-nudges (context args result caps)
  "Return bounded specialist nudges for Grep CONTEXT, ARGS, RESULT, and CAPS."
  (let ((pattern (plist-get args :pattern))
        (code-result-p
         (and (mevedel-specialist-nudges--grep-code-target-p args result)
              (not (mevedel-specialist-nudges--grep-comment-result-p result))
              (not (string-prefix-p "No matches found" result))))
        nudges)
    (when code-result-p
      (when (and (mevedel-specialist-nudges--identifier-like-pattern-p pattern)
                 (mevedel-specialist-nudges--family-applicable-p
                  context caps :xref
                  '("XrefReferences" "XrefDefinitions"))
                 (mevedel-specialist-nudges--nudge-allowed-p context :xref))
        (push (concat "For precise code symbol references, prefer `XrefReferences(identifier, file_path)'; for definitions or name discovery, prefer `XrefDefinitions(pattern, file_path)'."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context '("XrefReferences" "XrefDefinitions")
                           "xref")
                          ""))
              nudges))
      (when (and (mevedel-specialist-nudges--identifier-like-pattern-p pattern)
                 (mevedel-specialist-nudges--code-path-p (plist-get args :path))
                 (mevedel-specialist-nudges--family-applicable-p
                  context caps :imenu '("Imenu"))
                 (mevedel-specialist-nudges--nudge-allowed-p context :imenu))
        (push (concat "For a symbol outline in one known code file, prefer `Imenu(file_path)' over grepping the file for structure."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context '("Imenu") "imenu")
                          ""))
              nudges))
      (when (and (mevedel-specialist-nudges--structural-code-pattern-p pattern)
                 (mevedel-specialist-nudges--family-applicable-p
                  context caps :treesitter '("Treesitter"))
                 (mevedel-specialist-nudges--nudge-allowed-p context :treesitter))
        (push (concat "For syntax-node, AST, parent/child, or other structural code questions, prefer `Treesitter(file_path, line/column or whole_file)' over text search."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context '("Treesitter") "treesitter")
                          ""))
              nudges)))
    (nreverse nudges)))

(defun mevedel-specialist-nudges--read-specialist-nudges (context args result caps)
  "Return bounded specialist nudges for Read CONTEXT, ARGS, RESULT, and CAPS."
  (let ((path (plist-get args :file_path))
        nudges)
    (when (and (mevedel-specialist-nudges--code-path-p path)
               (not (mevedel-specialist-nudges--read-exact-range-p args))
               (not (string-prefix-p "File " result))
               (not (string-prefix-p "Error:" result)))
      (when (and (mevedel-specialist-nudges--family-applicable-p
                  context caps :imenu '("Imenu"))
                 (mevedel-specialist-nudges--nudge-allowed-p context :imenu))
        (push (concat "If your next step is symbol or structure discovery in this file, prefer `Imenu(file_path)' over reading or grepping more of the file."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context '("Imenu") "imenu")
                          ""))
              nudges))
      (when (and (mevedel-specialist-nudges--family-applicable-p
                  context caps :xref
                  '("XrefReferences" "XrefDefinitions"))
                 (mevedel-specialist-nudges--nudge-allowed-p context :xref))
        (push (concat "For follow-up code symbol lookup, prefer `XrefReferences(identifier, file_path)' or `XrefDefinitions(pattern, file_path)' instead of Grep."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context '("XrefReferences" "XrefDefinitions")
                          "xref")
                          ""))
              nudges))
      (when (and (mevedel-specialist-nudges--family-applicable-p
                  context caps :treesitter '("Treesitter"))
                 (mevedel-specialist-nudges--nudge-allowed-p context :treesitter))
        (push (concat "For syntax-node, AST, parent/child, or other structural questions in this file, prefer `Treesitter(file_path, line/column or whole_file)'."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context '("Treesitter") "treesitter")
                          ""))
              nudges))
      (when (and (string-suffix-p ".el" path)
                 (mevedel-specialist-nudges--family-applicable-p
                  context caps :elisp-introspection
                  '("function_source" "variable_source"
                    "function_documentation" "variable_documentation"
                    "library_source"))
                 (mevedel-specialist-nudges--nudge-allowed-p
                  context :elisp-introspection))
        (push (concat "For Emacs Lisp loaded-state questions, prefer introspection tools such as `function_source(function)', `variable_source(variable)', documentation/manual tools, or `library_source(library)'."
                      (or (mevedel-specialist-nudges--specialist-load-text
                           context
                           '("function_source" "variable_source"
                             "function_documentation" "variable_documentation"
                             "library_source")
                           "elisp")
                          ""))
              nudges)))
    (nreverse nudges)))

(defun mevedel-specialist-nudges-apply (context)
  "Return CONTEXT with bounded specialist-tool guidance appended."
  (let* ((tool (plist-get context :tool))
         (tool-name (and tool (mevedel-tool-name tool)))
         (args (plist-get context :args))
         (result (plist-get context :result))
         (session (plist-get context :session))
         (caps (mevedel-specialist-nudges--specialist-capabilities session))
         nudges)
    (when (and (stringp result)
               (not (string-prefix-p "Error:" result)))
      (pcase tool-name
        ("Grep"
         (setq nudges
               (mevedel-specialist-nudges--grep-specialist-nudges
                context args result caps)))
        ("Read"
         (setq nudges
               (mevedel-specialist-nudges--read-specialist-nudges
                context args result caps)))))
    (if nudges
        (plist-put
         context :result
         (concat result
                 "\n\n<system-reminder>\n"
                 (mapconcat #'identity nudges "\n")
                 "\n</system-reminder>"))
      context)))

(provide 'mevedel-specialist-nudges)
;;; mevedel-specialist-nudges.el ends here
