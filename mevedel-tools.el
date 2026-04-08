;;; mevedel-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel--parse-tools "ext:gptel-request" (backend tools))


;;
;;; Variables

(defvar mevedel-tools--util-tools
  '(;; Todo list
    ("mevedel" "TodoWrite")
    ("mevedel" "TodoRead")
    ;; Ask user
    ("mevedel" "Ask")
    ;; Directory access
    ("mevedel" "RequestAccess")
    ;; Agent
    ("mevedel" "Agent")
    ;; Tool search (deferred tool loading)
    ("mevedel" "ToolSearch")
    ;; Web search
    ("mevedel" "WebSearch")
    ("mevedel" "WebFetch")
    ("mevedel" "YouTube")))

(defvar mevedel-tools--read-tools
  '(;; File reading
    ("mevedel" "Read")
    ("mevedel" "Glob")
    ("mevedel" "Grep")
    ;; Websites
    ("mevedel" "WebFetch")))

(defvar mevedel-tools--code-tools
  '(;; Xref
    ("mevedel" "XrefReferences")
    ("mevedel" "XrefDefinitions")
    ;; Imenu
    ("mevedel" "Imenu")
    ;; Treesitter
    ("mevedel" "Treesitter")))

(defvar mevedel-tools--edit-tools
  '(;; File editing
    ("mevedel" "Write")
    ("mevedel" "Edit")
    ("mevedel" "Insert")
    ;; Create directory
    ("mevedel" "MkDir")))

(defvar mevedel-tools--eval-tools
  '(;; Bash
    ("mevedel" "Bash")
    ;; Eval
    ("mevedel" "Eval")))


;;
;;; Deferred Tool Loading (ToolSearch)

(defvar mevedel-tools--deferred-registry nil
  "Alist mapping tool paths to short descriptions for deferred loading.

Each entry is (TOOL-PATH . SHORT-DESCRIPTION) where TOOL-PATH is
a list like (\"mevedel\" \"Read\") suitable for `gptel-get-tool',
and SHORT-DESCRIPTION is a brief string for search results.

Tools in this registry are available but not loaded into the
active request until discovered via ToolSearch.")

(defvar mevedel-tools--deferred-descriptions
  '((("mevedel" "XrefReferences") . "Find all references to a symbol across the codebase using xref")
    (("mevedel" "XrefDefinitions") . "Jump to the definition of a symbol using xref")
    (("mevedel" "Imenu") . "List buffer symbols/definitions (functions, classes, variables) via imenu")
    (("mevedel" "Treesitter") . "Query syntax tree nodes using tree-sitter for structural code analysis")
    (("mevedel" "Write") . "Create a new file or completely rewrite an existing file")
    (("mevedel" "Edit") . "Replace text in a file using string matching or unified diff")
    (("mevedel" "Insert") . "Insert text at a specific line number in a file")
    (("mevedel" "MkDir") . "Create a new directory"))
  "Short descriptions for tools that can be deferred.

Used to populate `mevedel-tools--deferred-registry' based on
which tools are excluded from the active preset.")

(defvar-local mevedel-tools--pending-injections nil
  "List of `gptel-tool' structs to inject into the next request turn.

Set by the ToolSearch tool function, consumed by
`mevedel-tools--handle-deferred-inject' in the WAIT state handler.")

(defvar-local mevedel-tools--injected-tool-names nil
  "List of tool name strings that were injected by ToolSearch.

Tracked so they can be removed at the start of the next WAIT
cycle.  Deferred tools are per-turn only — the LLM must call
ToolSearch again to re-activate them.")

(defun mevedel-tools--handle-deferred-inject (fsm)
  "Manage deferred tool lifecycle in FSM's request payload.

This runs as a WAIT state handler, before `gptel--handle-wait'
fires the HTTP request.  It first removes any previously injected
deferred tools (they are per-turn only), then injects any new
tools queued by ToolSearch.

When tools change (removals or additions), the serialized tool
array in `info :data' is rebuilt from `info :tools' via
`gptel--parse-tools' to stay backend-agnostic.

NOTE: This assumes tools are at (plist-get data :tools) which is
true for OpenAI, Anthropic, Ollama, and Gemini backends.  Bedrock
uses a different nesting (:toolConfig :tools) and is not yet
supported."
  (let ((info (gptel-fsm-info fsm)))
    (when-let* ((buffer (plist-get info :buffer)))
      (with-current-buffer buffer
        (let ((changed nil))
          ;; Phase 1: Remove previously injected deferred tools
          (when mevedel-tools--injected-tool-names
            (let ((names mevedel-tools--injected-tool-names))
              (plist-put info :tools
                         (cl-remove-if (lambda (ts)
                                         (member (gptel-tool-name ts) names))
                                       (plist-get info :tools))))
            (setq mevedel-tools--injected-tool-names nil)
            (setq changed t))
          ;; Phase 2: Inject newly queued tools
          (when mevedel-tools--pending-injections
            (dolist (tool mevedel-tools--pending-injections)
              (let ((name (gptel-tool-name tool)))
                (unless (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) name))
                                    (plist-get info :tools))
                  (plist-put info :tools (cons tool (plist-get info :tools)))
                  (push name mevedel-tools--injected-tool-names))))
            (setq mevedel-tools--pending-injections nil)
            (setq changed t))
          ;; Phase 3: Re-serialize if tools changed
          (when changed
            (plist-put (plist-get info :data) :tools
                       (gptel--parse-tools (plist-get info :backend)
                                           (plist-get info :tools)))))))))

;;
;;; Deferred Tool Loading — Functions

(defun mevedel-tools--setup-deferred-registry (active-tool-paths)
  "Populate the deferred registry with tools not in ACTIVE-TOOL-PATHS.

ACTIVE-TOOL-PATHS is a list of tool paths (e.g. ((\"mevedel\" \"Read\") ...))
that are already loaded.  Any tool in
`mevedel-tools--deferred-descriptions' that is NOT in
ACTIVE-TOOL-PATHS gets added to `mevedel-tools--deferred-registry'."
  (setq mevedel-tools--deferred-registry
        (cl-remove-if
         (lambda (entry)
           (member (car entry) active-tool-paths))
         mevedel-tools--deferred-descriptions)))

(defun mevedel-tools--search-deferred (query)
  "Search the deferred tool registry for tools matching QUERY.

QUERY is split into whitespace-separated terms.  An entry matches
if ANY term appears as a substring in the tool name or its short
description.  Matching is case-insensitive.

Returns a list of (TOOL-PATH . SHORT-DESCRIPTION) pairs."
  (let ((terms (mapcar (lambda (t) (regexp-quote (downcase t)))
                       (split-string query nil t))))
    (cl-remove-if-not
     (lambda (entry)
       (let ((text (downcase (concat (cadr (car entry)) " " (cdr entry)))))
         (cl-some (lambda (term) (string-match-p term text)) terms)))
     mevedel-tools--deferred-registry)))

(cl-defun mevedel-tools--tool-search (callback query &optional load)
  "Search deferred tools matching QUERY, optionally LOAD them.

CALLBACK is the async callback.  QUERY is a search string matched
against tool names and descriptions.  When LOAD is non-nil (or
:json-false for false), matching tools are injected into the
current request for immediate use."
  (mevedel-tools--validate-params callback mevedel-tools--tool-search
                                  (query (stringp . "string"))
                                  (load booleanp nil))
  ;; Normalize boolean
  (when (eq load :json-false) (setq load nil))
  (let* ((matches (mevedel-tools--search-deferred query))
         (result
          (if matches
              (mapconcat
               (lambda (entry)
                 (format "- %s: %s" (cadr (car entry)) (cdr entry)))
               matches "\n")
            "No matching tools found.")))
    (when (and load matches)
      ;; Resolve tool structs from the registry and queue for injection
      (dolist (entry matches)
        (when-let* ((tool (ignore-errors (gptel-get-tool (car entry)))))
          (let ((tool-list (ensure-list tool)))
            (dolist (t1 tool-list)
              (unless (cl-find-if (lambda (pending)
                                    (equal (gptel-tool-name pending)
                                           (gptel-tool-name t1)))
                                  mevedel-tools--pending-injections)
                (push t1 mevedel-tools--pending-injections)))))))
    (funcall callback
             (if matches
                 (format "Found %d tool(s):\n%s%s"
                         (length matches) result
                         (if load "\n\nTools loaded and ready to use on the next turn."
                           "\n\nCall ToolSearch again with load=true to activate these tools."))
               result))))


(provide 'mevedel-tools)
;;; mevedel-tools.el ends here
