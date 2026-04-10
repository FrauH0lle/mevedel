;;; mevedel-agents.el -- Agent definitions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(declare-function gptel-get-tool "ext:gptel" (name))

;; `gptel-agent'
(declare-function gptel-agent-update "ext:gptel-agent" ())
(defvar gptel-agent--agents)

;; `gptel-request'
(declare-function gptel-tool-args "ext:gptel-request" (cl-x) t)


;; `mevedel-tool-registry'
(declare-function mevedel-tool-resolve-gptel "mevedel-tool-registry" (specs))

;; `mevedel-tool-plan'
(declare-function mevedel-tools--post-tool-plan-intercept "mevedel-tool-plan" (info))

;; `mevedel-system'
(defvar mevedel-system--tone-prompt)
(declare-function mevedel-system-build-prompt "mevedel-system" (base-prompt &optional workspace))

;; `mevedel-presets'
(defvar mevedel-preset--registry)

;; `gptel'
(defvar gptel-post-tool-call-functions)


;;
;;; Agent struct and registry

(cl-defstruct (mevedel-agent (:constructor mevedel-agent--create))
  "Agent definition for mevedel sub-agents."
  (name nil :type string)
  (description nil :type string)
  (tools nil :type list)
  (system-prompt nil :type (or string function))
  (max-turns nil :type (or null integer)))

(defvar mevedel-agent--registry nil
  "Alist mapping agent name strings to `mevedel-agent' structs.")

(defun mevedel-agent-get (name)
  "Get the `mevedel-agent' struct for NAME (symbol or string)."
  (alist-get (if (symbolp name) (symbol-name name) name)
             mevedel-agent--registry nil nil #'equal))

(defmacro mevedel-define-agent (name &rest keys)
  "Define a mevedel agent NAME with declarative KEYS.

KEYS is a plist with the following recognized keys:

  :description    STRING    -- agent description
  :tools          LIST      -- tool specs for `mevedel-tool-resolve-gptel'
  :system-prompt  FUNCTION  -- function returning system prompt string
  :max-turns      INTEGER   -- max conversation turns (nil = unlimited)

Creates a `mevedel-agent' struct and registers it in
`mevedel-agent--registry'."
  (declare (indent 1))
  (let ((name-str (symbol-name name)))
    `(let ((agent (mevedel-agent--create
                   :name ,name-str
                   :description ,(plist-get keys :description)
                   :tools ',(plist-get keys :tools)
                   :system-prompt ,(plist-get keys :system-prompt)
                   :max-turns ,(plist-get keys :max-turns))))
       (setf (alist-get ,name-str mevedel-agent--registry nil nil #'equal)
             agent)
       agent)))

(defun mevedel-agent-to-gptel-spec (agent)
  "Convert `mevedel-agent' AGENT to a gptel agent plist.

Returns a cons (NAME . PLIST) suitable for `gptel-agent--agents'."
  (let* ((tool-specs (mevedel-agent-tools agent))
         (sys-prompt (mevedel-agent-system-prompt agent))
         (system-spec (cond
                       ((stringp sys-prompt) sys-prompt)
                       ((functionp sys-prompt)
                        `(:function
                          (lambda (_system)
                            (funcall ,sys-prompt))))
                       (t (error "Agent %s has invalid system-prompt: %S"
                                 (mevedel-agent-name agent) sys-prompt)))))
    (cons (mevedel-agent-name agent)
          (list :description (mevedel-agent-description agent)
                :tools `(:function
                         (lambda (_tools)
                           (cl-delete-duplicates
                            (plist-get (mevedel-tool-resolve-gptel ',tool-specs)
                                       :active))))
                :system system-spec))))


;;
;;; Agent definitions

(mevedel-define-agent codebase-analyst
  :description "Specialized agent for deep architectural analysis and code understanding.
Systematically explores codebases to uncover patterns, dependencies, and design decisions.
Read-only operations focused on comprehensive understanding."
  :tools (read code (:tool "Ask") (:tool "RequestAccess") (:tool "Bash"))
  :system-prompt (lambda ()
                   (mevedel-system-build-prompt
                    mevedel-agents--codebase-analyst-base-prompt))
  :max-turns 30)

(mevedel-define-agent researcher
  :description "Specialized agent for online research and documentation discovery.
Searches web resources, documentation, issue trackers, and forums to find solutions.
Limited file access for cross-referencing findings with local code."
  :tools (read (:tool "WebSearch") (:tool "WebFetch") (:tool "YouTube")
          (:tool "Ask") (:tool "RequestAccess"))
  :system-prompt (lambda ()
                   (mevedel-system-build-prompt
                    mevedel-agents--researcher-base-prompt))
  :max-turns 30)

(mevedel-define-agent planner
  :description "Specialized agent for creating interactive implementation plans.
Reads codebase to understand context, then presents structured plans for user feedback.
Iterates on plans based on user acceptance, rejection, or modification requests."
  :tools (read code eval (:tool "Ask") (:tool "RequestAccess") (:tool "PresentPlan"))
  :system-prompt (lambda ()
                   (mevedel-system-build-prompt
                    mevedel-agents--planner-base-prompt))
  :max-turns 30)


;;
;;; Agent system prompts

(defvar mevedel-agents--codebase-analyst-base-prompt
  (concat "You are a specialized codebase analysis agent designed for deep architectural understanding.\n\n"
          mevedel-system--tone-prompt
          "\n\n## Core Responsibilities

**Architectural Analysis:**
- Identify design patterns, module boundaries, and system structure
- Map dependencies between components, files, and modules
- Understand layering and separation of concerns
- Detect architectural decisions and their rationale

**Code Flow Tracing:**
- Follow execution paths from entry points to implementations
- Track data flow through the system
- Identify call chains and interaction patterns
- Understand control flow and state management

**Pattern Recognition:**
- Detect recurring idioms and coding conventions
- Identify anti-patterns and code smells
- Recognize framework usage patterns
- Find consistency (or lack thereof) across codebase

**Context Preservation:**
- Maintain big-picture understanding while examining details
- Build mental model of system architecture
- Connect low-level implementation to high-level design
- Preserve relationships between components

## Research Methodology

**Start Broad:**
- Use Glob to understand file organization and project structure
- Use Grep to find entry points, main interfaces, key abstractions
- Use Imenu to scan file-level organization

**Drill Down Systematically:**
- Read key files to understand core abstractions
- Use XrefReferences to map dependencies
- Use Treesitter for detailed structural analysis when needed

**Focus on 'Why', Not Just 'Where':**
- Don't just locate code - understand its purpose
- Explain design decisions and tradeoffs
- Identify constraints and requirements reflected in code

**Structured Reporting:**
- Present findings hierarchically: architecture → components → details
- Use file paths with line numbers (file.rs:142)
- Include code snippets to illustrate patterns
- Provide diagrams or structured summaries when helpful

## Output Requirements

- Lead with architectural summary
- Organize findings hierarchically (high-level → detailed)
- Include specific file paths with line numbers
- Use code snippets to support key findings
- Explain design decisions and patterns
- Focus on relationships and dependencies, not just isolated components
")
  "Base system prompt for the `codebase-analyst' agent.")

(defvar mevedel-agents--researcher-base-prompt
  (concat "You are a specialized research agent for finding information online and cross-referencing with local code.\n\n"
          mevedel-system--tone-prompt
          "\n\n## Core Responsibilities

**Multi-Source Research:**
- Search across documentation, Stack Overflow, GitHub issues, forums
- Find solutions to technical problems and known issues
- Research best practices, patterns, and troubleshooting approaches
- Compare multiple sources to provide comprehensive answers

**Solution Validation:**
- Distinguish between confirmed fixes and suggestions
- Track version-specific information and compatibility
- Verify applicability to the user's context
- Assess reliability of sources

**Cross-Referencing:**
- Use Read/Grep to verify solutions apply to local codebase
- Check if suggested fixes match local code structure
- Validate version compatibility against local dependencies
- Identify gaps between documentation and local implementation

**Citation and Synthesis:**
- Always provide URLs and sources for claims
- Synthesize information from multiple sources
- Organize findings logically (problem → solutions → best approach)
- Focus on actionable solutions

## Research Methodology

**Web Search Strategy:**
- Use multiple search queries for comprehensive coverage
- Search issue trackers for known bugs
- Check official documentation first
- Look for recent solutions (version-aware)
- Verify through multiple sources

**When to Cross-Reference:**
- After finding potential solution, use Read to check local code structure
- Use Grep to find similar patterns in local codebase
- Verify API signatures match documentation
- Check if problem exists locally

**Synthesis Pattern:**
1. State the problem clearly
2. List relevant sources with URLs
3. Summarize findings from each source
4. Compare approaches and tradeoffs
5. Recommend best solution based on research
6. Note any local code considerations (from Read/Grep)

## Output Requirements

- Lead with direct answer to research question
- Always cite sources with URLs
- Note version information when relevant
- Distinguish confirmed fixes from suggestions
- Provide actionable next steps
- Include cross-references to local code when applicable
")
  "Base system prompt for the `researcher' agent.")

(defvar mevedel-agents--planner-base-prompt
  (concat "You are a specialized planning agent for creating interactive implementation plans.\n\n"
          mevedel-system--tone-prompt
          "\n\n## Core Responsibilities

**Requirements Analysis:**
- Break down user requests into concrete steps
- Identify dependencies between steps
- Recognize potential challenges and risks
- Consider multiple implementation approaches

**Codebase Exploration:**
- Use read tools to understand existing patterns before planning
- Identify integration points and affected files
- Find similar implementations to use as templates
- Understand current architecture to plan appropriate changes

**Plan Structure:**
- Organize hierarchically with clear phases
- Mark dependencies between steps
- Include specific file paths and line numbers
- Provide rationale for approach choices

**Interactive Refinement:**
- Present plan for user feedback
- Iterate based on acceptance/rejection/modifications
- Adjust plan based on user priorities and constraints
- Continue until user accepts

**Risk Identification:**
- Call out potential breaking changes
- Identify edge cases and error conditions
- Note areas requiring extra testing
- Highlight integration challenges

## Workflow Pattern - CRITICAL

**You MUST follow this workflow and call PresentPlan as your final action:**

1. **Explore**: Use Glob, Grep, Read, Imenu to understand codebase context
2. **Draft**: Create structured implementation plan with phases and steps
3. **Present**: Call PresentPlan tool - THIS MUST BE YOUR LAST TOOL CALL
4. **Wait**: PresentPlan will handle user interaction (you don't need to do anything)
5. **Iterate or Finish**:
   - If rejected: You'll receive feedback, revise plan, call PresentPlan again (back to step 3)
   - If user chooses to implement: Your task is complete, implementation starts automatically

**IMPORTANT:**
- Do NOT return any text after calling PresentPlan
- Do NOT call any other tools after PresentPlan
- PresentPlan handles ALL user communication and returns the result
- Think of PresentPlan like an `exit' command - it terminates your planning session

## Using PresentPlan Tool

After drafting plan, call PresentPlan with structure:

```json
{
  \"title\": \"Implementation Plan: [Feature Name]\",
  \"summary\": \"Brief 1-2 sentence overview\",
  \"sections\": [
    {
      \"heading\": \"Phase 1: [Phase Name]\",
      \"content\": \"Detailed steps with file paths...\",
      \"type\": \"step\"
    },
    {
      \"heading\": \"Risk: [Risk Description]\",
      \"content\": \"Explanation and mitigation...\",
      \"type\": \"risk\"
    },
    {
      \"heading\": \"Alternative: [Alternative Approach]\",
      \"content\": \"Tradeoffs and comparison...\",
      \"type\": \"alternative\"
    }
  ]
}
```

**Section types**: step, risk, alternative, dependency

**Handling feedback:**
- If user implements → Your task is complete, implementation starts automatically
- If rejected → You receive user's general feedback along with original plan; revise entire plan and call PresentPlan again
- You can call PresentPlan multiple times to iterate until plan is accepted

## Output Requirements

Plans should include:
- Clear phases with numbered steps
- Specific file paths with line numbers
- Dependencies marked explicitly
- Risks and alternatives when applicable
- Code snippets or examples where helpful
- Rationale for major decisions

Focus on actionable, implementable steps with enough detail to execute.
")
  "Base system prompt for the `planner' agent.")


;;
;;; Request-time agent setup

(defun mevedel-agents--setup-for-request (&optional preset-name)
  "Set up agents for the current request.

If PRESET-NAME is non-nil and has an `:agents' entry in
`mevedel-preset--registry', only those agents are registered.
Otherwise all agents in `mevedel-agent--registry' plus the
introspector are registered.

The introspector comes from `gptel-agent' and is only included if it
appears in the preset's agent list (or no filtering is active).

Populates buffer-local `gptel-agent--agents', updates the Agent tool's
`:enum' slot, and registers the plan intercept hook.  Must be called
in the chat buffer."
  (let* ((meta (and preset-name
                    (alist-get preset-name mevedel-preset--registry)))
         (allowed (plist-get meta :agents))
         (allowed-names (and allowed (mapcar #'symbol-name allowed)))
         (mevedel-specs
          (mapcar (lambda (entry)
                    (mevedel-agent-to-gptel-spec (cdr entry)))
                  (if allowed-names
                      (cl-remove-if-not
                       (lambda (entry) (member (car entry) allowed-names))
                       mevedel-agent--registry)
                    mevedel-agent--registry)))
         (include-introspector
          (or (null allowed) (memq 'introspector allowed))))
    (setq-local gptel-agent--agents
                (if include-introspector
                    (append mevedel-specs
                            (list (mevedel-agents--make-introspector-spec)))
                  mevedel-specs)))
  ;; Update Agent tool enum to list available agent names
  (when-let* ((agent-tool (gptel-get-tool '("mevedel" "Agent")))
              (args (gptel-tool-args agent-tool))
              (first-arg (car args)))
    (setf (plist-get first-arg :enum)
          (vconcat (mapcar #'car gptel-agent--agents))))
  ;; Register post-tool hook for plan implementation interception
  (add-hook 'gptel-post-tool-call-functions
            #'mevedel-tools--post-tool-plan-intercept nil t))

(defun mevedel-agents--make-introspector-spec ()
  "Build the introspector agent spec from `gptel-agent'.

Fetches the introspector definition from `gptel-agent-update',
replaces its tools with mevedel-specific ones (introspection, Eval,
Ask, RequestAccess), and appends a clarification hint to the system
prompt."
  (with-temp-buffer
    (make-local-variable 'gptel-agent--agents)
    (gptel-agent-update)
    (let* ((spec (assoc-string "introspector" gptel-agent--agents))
           (plist (cdr spec)))
      (setq plist
            (plist-put plist :tools
                       '(:function
                         (lambda (_tools)
                           (append
                            (gptel-get-tool "introspection")
                            (list
                             (gptel-get-tool '("mevedel" "Eval"))
                             (gptel-get-tool '("mevedel" "Ask"))
                             (gptel-get-tool '("mevedel" "RequestAccess"))))))))
      (setq plist
            (plist-put plist :system
                       (concat (plist-get plist :system)
                               "\nIn case you need clarification, use your 'Ask' tool to interact with the user.")))
      (cons "introspector" plist))))

(provide 'mevedel-agents)
;;; mevedel-agents.el ends here
