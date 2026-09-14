;;; test-mevedel-system.el --- Tests for mevedel-system.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests prompt assembly, component reports, workspace configuration loading,
;; and the live effective-prompt inspector.

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-execution-target)
(require 'mevedel-goal)
(require 'mevedel-structs)
(require 'mevedel-skills-prompt)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)
(require 'mevedel-utilities)
(require 'mevedel-system)
(require 'mevedel-agents)
(require 'mevedel-journal-index)
(require 'mevedel-context-delivery)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


(defun test-mevedel-system--profile (role)
  "Return a workspace-aware test profile with ROLE."
  `(:workspace-aware t
    :components ((role :text ,role)
                 memory-policy workspace-config memory environment skill-policy skills)))

;;
;;; Built-in profiles

(mevedel-deftest mevedel-system--provider-tool-schema ()
  ,test
  (test)
  :doc "uses provider JSON semantics for keyword properties and empty objects"
  (let* ((gptel--known-backends nil)
         (gptel--known-tools nil)
         (backend (gptel-make-openai "schema-inspector" :key "test" :models '(test)))
         (tools (list (gptel-make-tool
                       :name "Example" :description "Inspect a value"
                       :args '((:name "expression" :type string
                                      :description "A tool expression")
                               (:name "options" :type object :properties nil
                                      :optional t :description "Options")))))
         (actual (mevedel-system--provider-tool-schema backend tools))
         (parsed (json-parse-string actual :object-type 'hash-table))
         (properties (gethash "properties"
                              (gethash "parameters" (aref parsed 0)))))
    (should (equal (gptel--json-encode (gptel--parse-tools backend tools)) actual))
    (should (hash-table-p properties))
    (should (hash-table-p (gethash "properties" (gethash "options" properties))))))

(mevedel-deftest mevedel-system-build-prompt/retained ()
  ,test
  (test)
  :doc "preserves authored inline components with names matching dynamic components"
  (let ((profile '(:workspace-aware nil
                   :components ((memory :text "Authored inline context")))))
    (should (equal "Authored inline context"
                   (mevedel-system-build-prompt profile :retained t)))))

(mevedel-deftest mevedel-system-build-prompt/buddy
  (:doc "the buddy profiles render without a session")
  ,test
  (test)

  :doc "the review profile renders and tolerates a nil session"
  (let ((prompt (mevedel-system-build-prompt 'buddy :session nil)))
    (should (string-match-p "reviewing edits" prompt))
    (should (string-match-p "still typing" prompt))
    (should (string-match-p "read_buffer" prompt))
    (should (string-match-p "## Environment" prompt)))

  :doc "save guidance is delivered to main but not passive Buddy profiles"
  (let ((mevedel-memory-dirs nil))
    (should (string-search "## How to save memories"
                           (mevedel-system-build-prompt 'main)))
    (dolist (profile '(buddy buddy-guide))
      (let ((prompt (mevedel-system-build-prompt profile)))
        (should (string-search "Persistent memory" prompt))
        (should-not (string-search "## How to save memories" prompt)))))

  :doc "the guidance profile differs from the review profile in its role"
  (let ((review (mevedel-system-build-prompt 'buddy :session nil))
        (guide (mevedel-system-build-prompt 'buddy-guide :session nil)))
    (should (string-match-p "asking you what to build" guide))
    (should (string-match-p "Help develop the selected idea" guide))
    (should-not (string-match-p "Help develop the selected idea" review))
    (should-not (string-match-p "asking you what to build" review))
    ;; Only the role differs; the shared components are in both.
    (should (string-match-p "## Environment" guide))))

(mevedel-deftest mevedel-system-build-prompt/built-ins
  (:doc "built-in profiles select the main role without revision")
  (let ((main (mevedel-system-build-prompt 'main)))
    (should (string-match-p "Task execution protocol" main))
    (should (string-match-p "Tone and style" main))
    (should (string-match-p "Tool orchestration" main))
    (should (string-match-p "Untrusted tool content" main))
    (should (string-match-p "evidence to use for the user's task" main))
    (should (string-match-p "Do not weaken, delete, skip" main))
    (should (string-match-p "final permission denial" main))
    (should (string-match-p "automatic compaction" main))
    (should (string-match-p "VERDICT: PASS" main))
    (dolist (prompt (list main))
      (should (string-match-p "Resource addresses" prompt))
      (should (string-match-p "Read`, `Glob`, `Grep" prompt))
      (should (string-match-p "permitted `ApplyPatch`" prompt))
      (should (string-match-p "not an attachment, skill invocation, or delegation"
                             prompt))
      (should (string-match-p "user-composer syntax and[[:space:]]+do not execute"
                             prompt))
      (should (string-match-p "mevedel://" prompt))
      (should-not (string-match-p "mevedel://docs" prompt))
      (should-not (string-search "- `work://`" prompt))
      (dolist (scheme '("artifact://" "skill://" "agent://"
                        "history://" "mcp://"))
        (should-not (string-match-p (regexp-quote scheme) prompt)))
      (should-not (string-match-p "omp://" prompt)))
    (should-error (mevedel-system-build-prompt 'revise))))


;;
;;; Prompt builder

(mevedel-deftest mevedel-system-render-prompt-file ()
  ,test
  (test)
  :doc "renders the canonical guardian prompt referenced by the manual"
  (let ((guardian-doc
         (with-temp-buffer
           (insert-file-contents
            (file-name-concat
             (file-name-directory (locate-library "mevedel"))
             "docs" "guardian-prompts.md"))
           (buffer-string))))
    (dolist (prompt-path '("prompts/permissions/bash-guardian-system.md"))
      (let ((prompt (mevedel-system-render-prompt-file prompt-path)))
        (should-not (string-empty-p (string-trim prompt)))
        (should (string-search prompt-path guardian-doc))))))

(mevedel-deftest mevedel-inspect-effective-prompt ()
  ,test
  (test)
  :doc "reports the live prompt, session policy, and native and external tools"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-prompt-inspector-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project root root "prompt-inspector"))
         (session (mevedel-session-create "main" workspace root))
         (native-gptel
          (gptel-make-tool :name "NativeInspect" :category "mevedel"
                           :description "Native full description"
                           :args '((:name "path" :type string))))
         (native
          (mevedel-tool--create
           :name "NativeInspect" :category "mevedel"
           :prompt "Native full description"
           :prompt-source '(:kind file :path "/prompts/native.md")
           :gptel-tool native-gptel))
         (external
          (gptel-make-tool :name "ExternalInspect" :category "external"
                           :description "External full description"))
         (_loaded
          (gptel-make-tool :name "LoadedInspect" :category "external"
                           :description "Loaded full description"))
         (data (generate-new-buffer " *mevedel-prompt-inspector-data*"))
         inspector)
    (unwind-protect
        (progn
          (mevedel-tool-register native)
          (with-current-buffer data
            (setq-local mevedel--session session)
            (setq-local mevedel--workspace workspace)
            (setf (mevedel-session-preset-name session) 'mevedel-implement
                  (mevedel-session-permission-mode session) 'edits
                  (mevedel-session-sandbox-mode session) 'best-effort
                  (mevedel-session-tool-catalog session)
                  '((("external" "LoadedInspect") . "Inspect loaded state")))
            (setq-local gptel-system-prompt (lambda () "EXACT LIVE PROMPT"))
            (setq-local gptel-tools (list native-gptel external))
            (setq inspector (mevedel-inspect-effective-prompt)))
          (with-current-buffer inspector
            (let ((text (buffer-string)))
              (should buffer-read-only)
              (should (string-search "Preset: mevedel-implement" text))
              (should (string-search "Permission mode: edits" text))
              (should (string-search "EXACT LIVE PROMPT" text))
              (should (string-search "/prompts/native.md" text))
              (should (string-search "External full description" text))
              (should-not (string-search "Loaded full description" text))
              (should (string-search "external gptel tool" text))
              (should (string-search "Estimated total" text)))))
      (when (buffer-live-p inspector) (kill-buffer inspector))
      (when (buffer-live-p data) (kill-buffer data))
      (mevedel-tool-clear-registry)
      (delete-directory root t))))

(mevedel-deftest mevedel-system--resource-metadata
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-resource-metadata-" t))))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "returns nil when the request has no matching session"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "resource-metadata"))
         (context (mevedel-system-context--create
                   :workspace workspace
                   :working-directory root-dir)))
    (should-not (mevedel-system--resource-metadata context)))

  :doc "returns resource-owned metadata for a matching session"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "resource-metadata"))
         (session (mevedel-session-create "main" workspace root-dir))
         (context (mevedel-system-context--create
                   :workspace workspace
                   :working-directory root-dir
                   :session session))
         (metadata (mevedel-system--resource-metadata context)))
    (should (listp metadata))
    (should (plist-member metadata :roots))
    (should (plist-member metadata :agents))
    (should (plist-member metadata :memory-roots))))

(mevedel-deftest mevedel-system--resource-roster
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-resource-roster-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "always advertises packaged Mevedel documentation without a session"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "resource-roster"))
         (context (mevedel-system-context--create
                   :workspace workspace
                   :working-directory root-dir))
         (roster (mevedel-system--resource-roster context)))
    (should (string-match-p "mevedel://" roster))
    (should-not (string-match-p "mevedel://docs" roster))
    (should (string-match-p "packaged documentation" roster))
    (should (string-match-p "Read.*Glob.*Grep" roster)))

  :doc "advertises only resource families usable by the request context"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "resource-roster"))
         (session (mevedel-session-create "main" workspace root-dir))
         (context (mevedel-system-context--create
                   :workspace workspace
                   :working-directory root-dir
                   :session session)))
    (let ((mevedel-skill-dirs nil)
          (mevedel-skills-include-bundled nil))
      (cl-letf (((symbol-function 'mcp-hub-get-servers)
                 (lambda () nil)))
        (let ((roster (mevedel-system--resource-roster context)))
          (dolist (scheme '("work://" "artifact://" "memory://"))
            (should (string-match-p (regexp-quote scheme) roster)))
          (should (string-match-p "workspace-owned" roster))
          (should (string-match-p "ApplyPatch" roster))
          (should (string-match-p "mevedel://" roster))
          (should (string-search "history://saved" roster))
          (dolist (scheme '("skill://" "agent://" "history://root"
                            "mcp://"))
            (should-not (string-match-p (regexp-quote scheme) roster)))))))

  :doc "advertises root history without retained agents or reading content"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "root-history"))
         (session (mevedel-session-create "main" workspace root-dir))
         (context (mevedel-system-context--create
                   :workspace workspace :working-directory root-dir
                   :session session)))
    (with-temp-buffer
      (mevedel-session-set-root-buffer session (current-buffer))
      (cl-letf (((symbol-function 'mevedel-agent-conversation-project-history)
                 (lambda (&rest _) (ert-fail "Roster read history"))))
        (let ((roster (mevedel-system--resource-roster context)))
          (should (string-match-p "history://root`" roster))
          (should (string-match-p "history://root/PATH" roster))
          (should-not (string-match-p "agent://" roster)))))
    (should-not (string-match-p
                 "history://root" (mevedel-system--resource-roster context))))

  :doc "advertises configured resource families when their targets exist"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "resource-roster"))
         (session (mevedel-session-create "main" workspace root-dir))
         (save-path (file-name-as-directory
                     (make-temp-file "mevedel-resource-session-" t)))
         (memory-dir (file-name-concat root-dir ".mevedel" "memory"))
         (skill-dir (file-name-concat root-dir ".mevedel" "skills"
                                      "prompt-helper"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (skill (mevedel-skill--create
                 :name "prompt-helper"
                 :description "Prompt helper"
                 :source-file skill-file
                 :source-dir skill-dir
                 :active-p t
                 :model-invocable-p t)))
    (unwind-protect
        (progn
          (make-directory memory-dir t)
          (make-directory skill-dir t)
          (write-region "---\nname: prompt-helper\n---\n" nil skill-file)
          (setf (mevedel-session-save-path session) save-path
                (mevedel-session-skills session) (list skill))
          (let ((mevedel-skill-dirs nil)
                (mevedel-skills-include-bundled nil))
            (cl-letf (((symbol-function 'mcp-hub-get-servers)
                       (lambda () '((:name "docs" :status connected)))))
              (let ((roster
                     (mevedel-system--resource-roster
                      (mevedel-system-context--create
                       :workspace workspace
                       :working-directory root-dir
                       :session session))))
                (dolist (scheme '("work://" "artifact://" "skill://"
                                  "memory://" "mcp://" "mevedel://"))
                  (should (string-match-p (regexp-quote scheme) roster)))
                (dolist (alias '("skill://local-mevedel/SKILL"
                                 "skill://local-agents/SKILL"
                                 "skill://global-mevedel/SKILL"
                                 "skill://global-agents/SKILL"
                                 "skill://bundled/SKILL"
                                 "skill://managed/SKILL"
                                 "skill://plugin/PLUGIN/SKILL"))
                  (should (string-match-p (regexp-quote alias) roster)))
                (dolist (scheme '("agent://" "history://root"))
                  (should-not (string-match-p (regexp-quote scheme) roster)))))))
      (delete-directory save-path t))))

(mevedel-deftest mevedel-system--tool-orchestration-prompt
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-tool-orchestration-" t))))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "renders the context-specific resource roster in the orchestration prompt"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "tool-orchestration"))
         (session (mevedel-session-create "main" workspace root-dir))
         (context (mevedel-system-context--create
                   :workspace workspace
                   :working-directory root-dir
                   :session session))
         (prompt (cl-letf (((symbol-function 'mcp-hub-get-servers)
                            (lambda () nil)))
                   (let ((gptel-tools nil))
                     (mevedel-system--join-parts (mevedel-system--tool-orchestration-prompt context) (mevedel-system--resource-roster context))))))
    (should (string-match-p "Tool orchestration" prompt))
    (should (string-match-p "work://" prompt))
    (should (string-match-p "artifact://" prompt))
    (should-not (string-match-p "{{RESOURCE_ROSTER}}" prompt))
    (should-not (string-match-p "ToolCall" prompt))
    (should-not (string-match-p "{{PTC_GUIDANCE}}" prompt)))

  :doc "keeps earlier orchestration bytes stable when ToolCall becomes active"
  (let* ((workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "tool-orchestration"))
         (session (mevedel-session-create "main" workspace root-dir))
         (buffer (generate-new-buffer " *mevedel-ptc-guidance*")))
    (unwind-protect
        (cl-letf (((symbol-function 'mcp-hub-get-servers)
                   (lambda () nil)))
          (let* ((context (mevedel-system-context--create
                           :workspace workspace
                           :working-directory root-dir
                           :session session
                           :refresh-buffer buffer))
                 (before (mevedel-system--join-parts (mevedel-system--tool-orchestration-prompt context) (mevedel-system--resource-roster context))))
            (with-current-buffer buffer
              (setq-local gptel-tools
                          (list (gptel-make-tool :name "ToolCall"
                                                 :function #'ignore
                                                 :description "test"))))
            (should (equal before
                           (mevedel-system--join-parts (mevedel-system--tool-orchestration-prompt context) (mevedel-system--resource-roster context))))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-system-build-prompt
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-sys-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "includes base prompt, memory section, and environment info"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (prompt (mevedel-system-build-prompt
                  (test-mevedel-system--profile "BASE PROMPT CONTENT")
                  :workspace ws)))
    (should (string-match-p "BASE PROMPT CONTENT" prompt))
    (should (string-match-p "Persistent memory" prompt))
    (should (string-match-p "## Environment" prompt))
    (should (string-match-p "Emacs version:" prompt))
    (should (string-match-p (regexp-quote emacs-version) prompt))
    (should (string-match-p "<env>" prompt)))

  :doc "renders a remote session's cached readiness facts without reprobe"
  (let* ((remote-root (format "/ssh:user@host:%s" root-dir))
         (ws (mevedel-workspace--create
              :type 'project :id remote-root :root remote-root :name "remote"))
         (session (mevedel-session-create "main" ws))
         (target (mevedel-session-execution-target session)))
    (setf (mevedel-execution-target-readiness target)
          '(:status ready
            :operating-system "Linux"
            :operating-system-version "6.8.0-cached"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _args) (error "Unexpected executable lookup")))
              ((symbol-function 'process-file)
               (lambda (&rest _args) (error "Unexpected target process"))))
      (let ((prompt
             (mevedel-system-build-prompt
              '(:workspace-aware nil :components (environment))
              :workspace ws :session session)))
        (should (string-match-p "Working directory: .*mevedel-sys-" prompt))
        (should (string-match-p "Platform: linux" prompt))
        (should (string-match-p "OS Version: 6.8.0-cached" prompt)))))

  :doc "includes AGENTS.md content when present"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Use bun, not npm." nil agents-md)
    (let ((prompt (mevedel-system-build-prompt
                   (test-mevedel-system--profile "BASE")
                   :workspace ws)))
      (should (string-match-p "## Workspace Configuration" prompt))
      (should (string-match-p "Use bun, not npm\\." prompt))))

  :doc "orders stable content before dynamic memory and environment sections"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (memory-dir (file-name-concat root-dir ".mevedel" "memory"))
         (memory-file (file-name-concat memory-dir "MEMORY.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (make-directory memory-dir t)
    (write-region "Workspace guidance." nil agents-md)
    (write-region "Remembered fact." nil memory-file)
    (let* ((prompt (mevedel-system-build-prompt
                    (test-mevedel-system--profile "BASE")
                    :workspace ws))
           (base-pos (string-match-p "BASE" prompt))
           (config-pos (string-match-p "Workspace guidance\\." prompt))
           (memory-pos (string-match-p "Remembered fact\\." prompt))
           (env-pos (string-match-p "## Environment" prompt)))
      (should (< base-pos config-pos))
      (should (< config-pos memory-pos))
      (should (< memory-pos env-pos))))

  :doc "appends active skills after environment when a session exposes them"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "review-spec"
                 :description "Review a spec"
                 :active-p t
                 :model-invocable-p t)))
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (let* ((prompt (mevedel-system-build-prompt
                      (test-mevedel-system--profile "BASE")
                      :workspace ws
                      :session session
                      :refresh-buffer (current-buffer)))
             (env-pos (string-match-p "## Environment" prompt))
             (skills-pos (string-match-p "## Skills" prompt)))
        (should (string-match-p "^- review-spec: Review a spec$" prompt))
        (should (string-match-p "(Skill :name" prompt))
        (should (and env-pos skills-pos))
        (should (< env-pos skills-pos)))))

  :doc "omits active skills when session cwd differs from prompt context"
  (let* ((subdir (file-name-concat root-dir "sub"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (session (mevedel-session-create "main" ws subdir))
         (skill (mevedel-skill--create
                 :name "review-spec"
                 :description "Review a spec"
                 :active-p t
                 :model-invocable-p t)))
    (make-directory subdir t)
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (let ((prompt (mevedel-system-build-prompt
                     (test-mevedel-system--profile "BASE")
                     :workspace ws
                     :working-directory root-dir
                     :session session
                     :refresh-buffer (current-buffer))))
        (should-not (string-match-p "## Skills" prompt)))))

  :doc "renders active Goal context only for the matching workspace and cwd"
  (let* ((other-root (file-name-as-directory
                      (make-temp-file "mevedel-sys-other-" t)))
         (subdir (file-name-concat root-dir "sub"))
         (workspace (mevedel-workspace-get-or-create
                     'project root-dir root-dir "sysproj"))
         (other-workspace (mevedel-workspace-get-or-create
                           'project other-root other-root "other"))
         (session (mevedel-session-create "main" workspace root-dir))
         (other-session
          (mevedel-session-create "other" other-workspace other-root))
         (cwd-session (mevedel-session-create "cwd" workspace subdir))
         (profile '(:workspace-aware nil :components (active-goal))))
    (unwind-protect
        (progn
          (make-directory subdir t)
          (dolist (owned-session (list session other-session cwd-session))
            (setf (mevedel-session-goal owned-session)
                  (mevedel-goal--create
                   :id "goal" :objective "PRIVATE GOAL" :status 'active
                   :tokens-used 0 :time-used-seconds 0 :turns-run 0
                   :created-at "now" :updated-at "now")))
          (should (string-match-p
                   "PRIVATE GOAL"
                   (mevedel-system-build-prompt
                    profile :workspace workspace :working-directory root-dir
                    :session session)))
          (should-not (string-match-p
                       "PRIVATE GOAL"
                       (mevedel-system-build-prompt
                        profile :workspace workspace
                        :working-directory root-dir :session other-session)))
          (should-not (string-match-p
                       "PRIVATE GOAL"
                       (mevedel-system-build-prompt
                        profile :workspace workspace
                        :working-directory root-dir :session cwd-session))))
      (delete-directory other-root t)))

  :doc "ignores CLAUDE.md when AGENTS.md is absent"
  (let* ((claude-md (file-name-concat root-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Claude-specific guidance." nil claude-md)
    (let ((prompt (mevedel-system-build-prompt
                   (test-mevedel-system--profile "BASE")
                   :workspace ws)))
      (should-not (string-match-p "## Workspace Configuration" prompt))
      (should-not (string-match-p "Claude-specific guidance" prompt))))

  :doc "uses AGENTS.md and ignores CLAUDE.md when both files exist"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (claude-md (file-name-concat root-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "AGENTS wins." nil agents-md)
    (write-region "CLAUDE loses." nil claude-md)
    (let ((prompt (mevedel-system-build-prompt
                   (test-mevedel-system--profile "BASE")
                   :workspace ws)))
      (should (string-match-p "AGENTS wins" prompt))
      (should-not (string-match-p "CLAUDE loses" prompt))))

  :doc "loads AGENTS.local.md after the shared file in the same directory"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (local-md (file-name-concat root-dir "AGENTS.local.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Shared guidance." nil agents-md)
    (write-region "Private guidance." nil local-md)
    (let* ((prompt (mevedel-system-build-prompt
                    (test-mevedel-system--profile "BASE")
                    :workspace ws))
           (shared-pos (string-match-p "Shared guidance\\." prompt))
           (private-pos (string-match-p "Private guidance\\." prompt)))
      (should shared-pos)
      (should private-pos)
      (should (< shared-pos private-pos))))

  :doc "layers instruction files from workspace root to working directory"
  (let* ((module-dir (file-name-concat root-dir "packages" "api"))
         (root-agents (file-name-concat root-dir "AGENTS.md"))
         (module-agents (file-name-concat module-dir "AGENTS.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (make-directory module-dir t)
    (write-region "Root guidance." nil root-agents)
    (write-region "Module guidance." nil module-agents)
    (let* ((prompt (mevedel-system-build-prompt
                    (test-mevedel-system--profile "BASE")
                    :workspace ws
                    :working-directory module-dir))
           (root-pos (string-match-p "Root guidance\\." prompt))
           (module-pos (string-match-p "Module guidance\\." prompt)))
      (should root-pos)
      (should module-pos)
      (should (< root-pos module-pos))))

  :doc "layers local instruction files with their directory scope"
  (let* ((module-dir (file-name-concat root-dir "packages" "cli"))
         (root-agents (file-name-concat root-dir "AGENTS.md"))
         (root-local (file-name-concat root-dir "AGENTS.local.md"))
         (module-agents (file-name-concat module-dir "AGENTS.md"))
         (module-local (file-name-concat module-dir "AGENTS.local.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (make-directory module-dir t)
    (write-region "Root shared." nil root-agents)
    (write-region "Root local." nil root-local)
    (write-region "Module shared." nil module-agents)
    (write-region "Module local." nil module-local)
    (let* ((prompt (mevedel-system-build-prompt
                    (test-mevedel-system--profile "BASE")
                    :workspace ws
                    :working-directory module-dir))
           (root-shared-pos (string-match-p "Root shared\\." prompt))
           (root-local-pos (string-match-p "Root local\\." prompt))
           (module-shared-pos (string-match-p "Module shared\\." prompt))
           (module-local-pos (string-match-p "Module local\\." prompt)))
      (should root-shared-pos)
      (should root-local-pos)
      (should module-shared-pos)
      (should module-local-pos)
      (should (< root-shared-pos root-local-pos))
      (should (< root-local-pos module-shared-pos))
      (should (< module-shared-pos module-local-pos))))

  :doc "ignores CLAUDE.md in each layered directory"
  (let* ((module-dir (file-name-concat root-dir "packages" "web"))
         (root-claude (file-name-concat root-dir "CLAUDE.md"))
         (module-agents (file-name-concat module-dir "AGENTS.md"))
         (module-claude (file-name-concat module-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (make-directory module-dir t)
    (write-region "Root Claude guidance." nil root-claude)
    (write-region "Module AGENTS guidance." nil module-agents)
    (write-region "Module Claude loses." nil module-claude)
    (let ((prompt (mevedel-system-build-prompt
                   (test-mevedel-system--profile "BASE")
                   :workspace ws
                   :working-directory module-dir)))
      (should (string-match-p "Module AGENTS guidance\\." prompt))
      (should-not (string-match-p "Root Claude guidance\\." prompt))
      (should-not (string-match-p "Module Claude loses\\." prompt))))

  :doc "omits Workspace Configuration when neither file exists"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (prompt (mevedel-system-build-prompt
                  (test-mevedel-system--profile "BASE")
                  :workspace ws)))
    (should-not (string-match-p "## Workspace Configuration" prompt)))

  :doc "does not reuse a different base prompt from the section cache"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (_prompt-one
          (mevedel-system-build-prompt
           (test-mevedel-system--profile "BASE ONE")
           :workspace ws))
         (prompt-two
          (mevedel-system-build-prompt
           (test-mevedel-system--profile "BASE TWO")
           :workspace ws)))
    (should (string-match-p "BASE TWO" prompt-two))
    (should-not (string-match-p "BASE ONE" prompt-two))))

;;
;;; Persistent memory

(mevedel-deftest mevedel-system-build-prompt/memory
  (:vars* ((root (file-name-as-directory (make-temp-file "mevedel-memory-flow-" t)))
           (mevedel-memory-dirs '(".agents/memory/"))
           (index (file-name-concat root ".agents/memory/MEMORY.md"))
           (workspace (mevedel-workspace--create
                       :type 'project :id root :root root :name "memory")))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "unchanged memory context stays identical across calendar days"
  (let ((now (encode-time 0 0 12 8 5 2026)) first second)
    (make-directory (file-name-directory index) t)
    (with-temp-file index (insert "- [Release](release.md) - release coordination\n"))
    (set-file-times index now)
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (let ((mevedel-system--prompt-component-cache (make-hash-table :test #'equal)))
        (setq first (mevedel-system-build-prompt
                     '(:workspace-aware nil :components (memory)) :workspace workspace)))
      (setq now (time-add now (days-to-time 1)))
      ;; Fresh memoization makes this a rendered-content check, not a cache hit.
      (let ((mevedel-system--prompt-component-cache (make-hash-table :test #'equal)))
        (setq second (mevedel-system-build-prompt
                      '(:workspace-aware nil :components (memory)) :workspace workspace))))
    (should (equal first second))
    (should (string-search "2026-05-08" second)))

  :doc "changed index refreshes context without rewriting earlier save guidance"
  (let ((mevedel-system--prompt-component-cache (make-hash-table :test #'equal))
        (profile '(:workspace-aware nil :components (memory-policy memory-save-policy memory)))
        first second)
    (make-directory (file-name-directory index) t)
    (with-temp-file index (insert "- [Release](release.md) - old coordination\n"))
    (setq first (mevedel-system-build-prompt profile :workspace workspace))
    (with-temp-file index (insert "- [Release](release.md) - revised release coordination\n"))
    (setq second (mevedel-system-build-prompt profile :workspace workspace))
    (should (string-search "revised release coordination" second))
    (should-not (string-search "old coordination" second))
    (should (equal (substring first 0 (string-search "## Memory context" first))
                   (substring second 0 (string-search "## Memory context" second))))))

(mevedel-deftest mevedel-system--memory-content
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-memory-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "reports absent indexes without inventing a save task"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (content (mevedel-system--memory-content ws)))
    (should (string-match-p "memory indexes are currently empty" content))
    (should-not (string-match-p "save" content)))

  :doc "adds an absolute modification date and truncates MEMORY.md to 200 lines"
  (let* ((memory-dir (file-name-concat root-dir ".mevedel" "memory"))
         (memory-file (file-name-concat memory-dir "MEMORY.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (make-directory memory-dir t)
    (with-temp-file memory-file
      (dotimes (i 205)
        (insert (format "line-%03d\n" (1+ i)))))
    (let ((content (mevedel-system--memory-content ws)))
      (should (string-match-p
               "<!-- Last updated: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} -->"
               content))
      (should (string-match-p "line-001" content))
      (should (string-match-p "line-200" content))
      (should-not (string-match-p "line-201" content))))

  :doc "merges existing memory indexes in configured order with labels"
  (let* ((local-mevedel (file-name-concat root-dir ".mevedel" "memory"))
         (local-agents (file-name-concat root-dir ".agents" "memory"))
         (global-mevedel (file-name-concat root-dir "global-mevedel"))
         (global-agents (file-name-concat root-dir "global-agents"))
         (mevedel-memory-dirs
          (list ".mevedel/memory/" ".agents/memory/"
                global-mevedel global-agents))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (dolist (dir (list local-mevedel local-agents global-mevedel global-agents))
      (make-directory dir t))
    (write-region "local mevedel fact" nil
                  (file-name-concat local-mevedel "MEMORY.md"))
    (write-region "local agents fact" nil
                  (file-name-concat local-agents "MEMORY.md"))
    (write-region "global mevedel fact" nil
                  (file-name-concat global-mevedel "MEMORY.md"))
    (write-region "global agents fact" nil
                  (file-name-concat global-agents "MEMORY.md"))
    (let* ((content (mevedel-system--memory-content ws))
           (local-mevedel-pos (string-match-p "local mevedel fact" content))
           (local-agents-pos (string-match-p "local agents fact" content))
           (global-mevedel-pos (string-match-p "global mevedel fact" content))
           (global-agents-pos (string-match-p "global agents fact" content)))
      (should (string-match-p "Local mevedel memory" content))
      (should (string-match-p "Local agents memory" content))
      (should (< local-mevedel-pos local-agents-pos))
      (should (< local-agents-pos global-mevedel-pos))
      (should (< global-mevedel-pos global-agents-pos)))))

(mevedel-deftest mevedel-system--memory-context-prompt
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-memory-prompt-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/" ".agents/memory/")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "includes configured roots and index state separately from save policy"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (prompt (mevedel-system--memory-context-prompt ws)))
    (should (string-match-p (regexp-quote
                             (file-name-concat root-dir
                                               ".mevedel" "memory"))
                            prompt))
    (should (string-match-p (regexp-quote
                             (file-name-concat root-dir
                                               ".agents" "memory"))
                            prompt))
    (should (string-search "memory indexes are currently empty" prompt))
    (should-not (string-search "How to save memories" prompt))))

(mevedel-deftest mevedel-system--memory-cache-key
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-memory-cache-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/" ".agents/memory/")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "keys configured index metadata and the context template without calendar state"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (context (mevedel-system-context--create
                   :workspace ws
                   :working-directory root-dir))
         (key (mevedel-system--memory-cache-key context)))
    (should (= 2 (length (plist-get key :files))))
    (should (string-suffix-p "prompts/system/memory-context.md"
                            (plist-get (plist-get key :template) :file)))
    (should-not (plist-member key :date))))


;;
;;; Prompt profiles

(mevedel-deftest mevedel-system-build-prompt/profile
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-profile-" t))))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "renders registered and inline components in exact profile order"
  (let ((mevedel-system--prompt-components nil)
        (mevedel-system--prompt-profiles nil)
        (mevedel-system--prompt-component-cache
         (make-hash-table :test #'equal))
        (mevedel-system--source-dir root-dir)
        (prompt-file (file-name-concat root-dir "role.md")))
    (write-region "from file" nil prompt-file)
    (mevedel-define-prompt-component registered-text :text "registered")
    (mevedel-define-prompt-component blank
      :producer (lambda (_context) " \n"))
    (mevedel-define-prompt-profile sample
      :workspace-aware nil
      :components '(registered-text
                    (role :file "role.md")
                    blank
                    (tail :text "inline")))
    (should
     (equal (mevedel-system-build-prompt 'sample)
            "registered\n\nfrom file\n\ninline")))

  :doc "memoizes keyed producers and invalidates them on re-registration"
  (let ((mevedel-system--prompt-components nil)
        (mevedel-system--prompt-profiles nil)
        (mevedel-system--prompt-component-cache
         (make-hash-table :test #'equal))
        (cache-key 'same)
        (calls 0))
    (mevedel-define-prompt-component cached
      :cache 'keyed
      :cache-key (lambda (_context) cache-key)
      :producer (lambda (_context)
                  (setq calls (1+ calls))
                  (format "call-%d" calls)))
    (mevedel-define-prompt-profile sample
      :workspace-aware nil
      :components '(cached))
    (should (equal (mevedel-system-build-prompt 'sample) "call-1"))
    (should (equal (mevedel-system-build-prompt 'sample) "call-1"))
    (should (= calls 1))
    (setq cache-key 'changed)
    (should (equal (mevedel-system-build-prompt 'sample) "call-2"))
    (mevedel-define-prompt-component cached
      :producer (lambda (_context) "replacement"))
    (should (equal (mevedel-system-build-prompt 'sample) "replacement")))

  :doc "rejects unknown, duplicate, malformed, and incomplete profiles"
  (let ((mevedel-system--prompt-components nil)
        (mevedel-system--prompt-profiles nil)
        (mevedel-system--prompt-component-cache
         (make-hash-table :test #'equal)))
    (mevedel-define-prompt-component role :text "role")
    (mevedel-define-prompt-component workspace-config :text "config")
    (mevedel-define-prompt-component environment :text "environment")
    (should-error
     (mevedel-define-prompt-component invalid-file :file nil))
    (should-error
     (mevedel-define-prompt-component invalid-cache
       :cache 'forever
       :text "cached"))
    (dolist (profile
             '((:workspace-aware nil :components (missing))
               (:workspace-aware nil :components (role role))
               (:workspace-aware nil :components ((role :text "a" :file "b")))
               (:workspace-aware t :components (role environment))
               (:workspace-aware t
                :components ((workspace-config :text "fake config")
                             (environment :text "fake environment")))))
      (should-error (mevedel-system-build-prompt profile)))))

(mevedel-deftest mevedel-system-prompt-component-report
  (:doc "reports components in profile order and whether cache was warm")
  (let ((mevedel-system--prompt-components nil)
        (mevedel-system--prompt-profiles nil)
        (mevedel-system--prompt-component-cache
         (make-hash-table :test #'equal)))
    (mevedel-define-prompt-component cached
      :cache 'global
      :producer (lambda (_context) "cached text"))
    (mevedel-define-prompt-profile sample
      :workspace-aware nil
      :components '((role :text "role text") cached))
    (let ((cold (mevedel-system-prompt-component-report 'sample)))
      (should (equal (mapcar (lambda (entry) (plist-get entry :name)) cold)
                     '(role cached)))
      (should-not (plist-get (cadr cold) :cached)))
    (let ((warm (mevedel-system-prompt-component-report 'sample)))
      (should (plist-get (cadr warm) :cached))
      (should (= (plist-get (car warm) :chars) 9)))))

(mevedel-deftest mevedel-system-build-prompt/journal ()
  ,test
  (test)
  :doc "pushes the map only to main, while worker retains memory and read-only agents report lessons"
  (let* ((mevedel-journal-max-age-days nil)
         (root (make-temp-file "mevedel-journal-profile-" t))
         (workspace (mevedel-workspace--create
                     :root root :journal-observation
                     (list :root root :time (float-time) :entries
                           (list (list :kind 'digest :id (make-string 64 ?a)
                                       :file (concat "2026-09-07T12:00:00Z-" (make-string 64 ?a) ".md")
                                       :created "2026-09-07T12:00:00Z" :session-name "Source"
                                       :body "## Done\n- none\n\n## Learned\n- User: Map-only lesson\n\n## Surprised\n- none\n\n## Unfinished\n- none")))))
         (session (mevedel-session--create :workspace workspace :working-directory root)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((prompt (mevedel-system-build-prompt 'main :workspace workspace
                                                    :working-directory root :session session)))
            (should (string-match-p "Recent journal evidence" prompt))
            (should (string-match-p "Map-only lesson" prompt))
            (should (string-match-p "work://shared/" prompt))
            (dolist (guidance '("main agent, subagents, and later sessions in the same workspace"
                                "mevedel-created worktree sessions"
                                "not automatically added to conversations"
                                "give the receiving agent the note's address"))
              (should (string-search guidance prompt))))
          (let ((prompt (mevedel-system-build-prompt 'main :retained t
                                                    :workspace workspace :session session)))
            (should-not (string-search "Map-only lesson" prompt)))
          (dolist (name '("worker" "explorer" "verifier" "reviewer"))
            (let* ((agent (mevedel-agent-freeze (mevedel-agent-get name)))
                   (prompt (mevedel-agent-system-prompt agent))
                   (fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
              (setq-local mevedel--agent-invocation
                          (mevedel-agent-invocation--create :agent agent))
              (should-not (string-match-p "Recent journal evidence\\|Map-only lesson" prompt))
              (should-not (memq 'journal (mevedel-agent-context-components agent)))
              (mevedel-context-delivery-stage fsm)
              (let ((context (prin1-to-string
                              (plist-get (gptel-fsm-info fsm) :mevedel-reminder-entries))))
                (should (string-search "memory://journal/" context))
                (should-not (string-search "Map-only lesson" context))
                (should (string-search "work://shared/" context))
                (dolist (guidance '("main agent, subagents, and later sessions in the same workspace"
                                    "mevedel-created worktree sessions"
                                    "not automatically added to conversations"
                                    "give the receiving agent the note's address"))
                  (should (string-search guidance context))))
              (if (equal name "worker")
                  (should (string-search "Persistent memory" prompt))
                (should (string-search "source/task attribution" prompt)))))
          (dolist (profile '(bash-guardian buddy buddy-guide))
            (let ((prompt (mevedel-system-build-prompt profile :workspace workspace
                                                      :working-directory root :session session)))
              (should-not (string-match-p "Recent journal evidence\\|Map-only lesson" prompt)))))
      (delete-directory root t))))

(provide 'test-mevedel-system)
;;; test-mevedel-system.el ends here
