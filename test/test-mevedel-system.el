;;; test-mevedel-system.el --- Tests for mevedel-system.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests prompt assembly, component reports, workspace configuration loading,
;; and the live effective-prompt inspector.

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-structs)
(require 'mevedel-skills-prompt)
(require 'mevedel-tool-registry)
(require 'mevedel-workspace)
(require 'mevedel-utilities)
(require 'mevedel-system)
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
                 workspace-config memory environment skills)))

(defun test-mevedel-system--resource-profile (role)
  "Return a test profile with ROLE and resource-address guidance."
  `(:workspace-aware t
    :components ((role :text ,role)
                 tool-orchestration
                 workspace-config memory environment skills)))


;;
;;; Built-in profiles

(mevedel-deftest mevedel-system-build-prompt/built-ins
  (:doc "built-in profiles select main and tutor roles without revision")
  (let ((main (mevedel-system-build-prompt 'main))
        (tutor (mevedel-system-build-prompt 'tutor)))
    (should (string-match-p "Task execution protocol" main))
    (should (string-match-p "Tone and style" main))
    (should (string-match-p "Tool orchestration" main))
    (should (string-match-p "Untrusted tool content" main))
    (should (string-match-p "evidence to use for the user's task" main))
    (should (string-match-p "Do not weaken, delete, skip" main))
    (should (string-match-p "final permission denial" main))
    (should (string-match-p "automatic compaction" main))
    (should (string-match-p "VERDICT: PASS" main))
    (dolist (prompt (list main tutor))
      (should (string-match-p "Resource addresses" prompt))
      (should (string-match-p "Read`, `Glob`, `Grep" prompt))
      (should (string-match-p "permitted `ApplyPatch`" prompt))
      (should (string-match-p
               (regexp-quote "Skill(name=...)")
               prompt))
      (should (string-match-p
               (regexp-quote "Agent(...)")
               prompt))
      (should (string-match-p "SendMessage" prompt))
      (should (string-match-p "short notifications" prompt))
      (should (string-match-p "not an attachment,[[:space:]]+invocation, or delegation"
                             prompt))
      (should (string-match-p "user-composer syntax and[[:space:]]+do not execute"
                             prompt))
      (dolist (scheme '("local://" "artifact://" "skill://" "agent://"
                        "history://" "memory://" "mcp://"))
        (should-not (string-match-p (regexp-quote scheme) prompt)))
      (should-not (string-match-p "omp://" prompt)))
    (should (string-match-p "NEVER PROVIDE SOLUTIONS" tutor))
    (should (string-match-p "Tutoring style" tutor))
    (should-not (string-match-p "Tone and style" tutor))
    (should-error (mevedel-system-build-prompt 'revise))))


;;
;;; Prompt builder

(mevedel-deftest mevedel-system-render-prompt-file ()
  ,test
  (test)
  :doc "keeps maintained guardian prompt contracts synchronized exactly"
  (let ((guardian-doc
         (with-temp-buffer
           (insert-file-contents
            (file-name-concat
             (file-name-directory (locate-library "mevedel"))
             "docs" "guardian-prompts.md"))
           (buffer-string))))
    (dolist (prompt-path '("prompts/permissions/bash-guardian-system.md"))
      (let ((prompt (mevedel-system-render-prompt-file prompt-path)))
        (should
         (string-match-p
          (regexp-quote (concat "```text\n" prompt "```"))
          guardian-doc))))))

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
                  (mevedel-session-sandbox-mode session) 'best-effort)
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
              (should (string-search "external gptel tool" text))
              (should (string-search "Estimated total" text)))))
      (when (buffer-live-p inspector) (kill-buffer inspector))
      (when (buffer-live-p data) (kill-buffer data))
      (mevedel-tool-clear-registry)
      (delete-directory root t))))

(mevedel-deftest mevedel-system-build-prompt
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-sys-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/")))
   :after-each (delete-directory root-dir t))
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

  :doc "advertises only resource families usable by the request context"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (session (mevedel-session-create "main" ws root-dir)))
    (let ((mevedel-skill-dirs nil)
          (mevedel-skills-include-bundled nil))
      (cl-letf (((symbol-function 'mcp-hub-get-servers)
                 (lambda () nil)))
        (let ((prompt (mevedel-system-build-prompt
                       (test-mevedel-system--resource-profile "BASE")
                       :workspace ws
                       :session session
                       :refresh-buffer (current-buffer))))
          (dolist (scheme '("local://" "artifact://"))
            (should (string-match-p (regexp-quote scheme) prompt)))
          (dolist (scheme '("skill://" "agent://" "history://"
                            "memory://" "mcp://"))
            (should-not (string-match-p (regexp-quote scheme) prompt)))))))

  :doc "advertises configured resource families when their targets exist"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (session (mevedel-session-create "main" ws root-dir))
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
              (let ((prompt (mevedel-system-build-prompt
                             (test-mevedel-system--resource-profile "BASE")
                             :workspace ws
                             :session session
                             :refresh-buffer (current-buffer))))
                (dolist (scheme '("local://" "artifact://" "skill://"
                                  "memory://" "mcp://"))
                  (should (string-match-p (regexp-quote scheme) prompt)))
                (dolist (scheme '("agent://" "history://"))
                  (should-not (string-match-p (regexp-quote scheme) prompt)))))))
      (delete-directory save-path t)))

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
        (should (string-match-p "Skill(name=\\.\\.\\.)" prompt))
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

(mevedel-deftest mevedel-system--human-time-age
  (:doc "`mevedel-system--human-time-age' formats today/yesterday/day counts")
  (let ((now (encode-time 0 0 12 8 5 2026)))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (should (equal "today" (mevedel-system--human-time-age now)))
      (should (equal "yesterday"
                     (mevedel-system--human-time-age
                      (time-subtract now (days-to-time 1)))))
      (should (equal "4 days ago"
                     (mevedel-system--human-time-age
                      (time-subtract now (days-to-time 4))))))))

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
  :doc "returns empty index guidance when MEMORY.md is absent"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (content (mevedel-system--memory-content ws)))
    (should (string-match-p "memory indexes are currently empty" content))
    (should (string-match-p "separate topic files" content)))

  :doc "adds age metadata and truncates MEMORY.md to 200 lines"
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
               "<!-- Last updated: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} (today) -->"
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

(mevedel-deftest mevedel-system--memory-prompt
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-memory-prompt-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/" ".agents/memory/")))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "includes memory root routing rules and configured roots"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (prompt (funcall mevedel-system--memory-prompt ws)))
    (should (string-match-p (regexp-quote
                             (file-name-concat root-dir
                                               ".mevedel" "memory"))
                            prompt))
    (should (string-match-p (regexp-quote
                             (file-name-concat root-dir
                                               ".agents" "memory"))
                            prompt))
    (should (string-match-p "If an existing memory covers the topic" prompt))
    (should (string-match-p "global memory unless the user asks" prompt))
    (should (string-match-p "local memory unless the user asks" prompt))
    (should (string-match-p "Prefer `.agents/memory/`" prompt))
    (should (string-match-p "Use `.mevedel/memory/`" prompt))))

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
  :doc "includes the current date so age metadata refreshes daily"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (context (mevedel-system-context--create
                   :workspace ws
                   :working-directory root-dir))
         key-one key-two)
    (cl-letf (((symbol-function 'mevedel-system--current-date)
               (lambda () "2026-05-08")))
      (setq key-one (mevedel-system--memory-cache-key context)))
    (cl-letf (((symbol-function 'mevedel-system--current-date)
               (lambda () "2026-05-09")))
      (setq key-two (mevedel-system--memory-cache-key context)))
    (should-not (equal key-one key-two))
    (should (= 2 (length (plist-get key-one :files))))
    (should (member :date key-one))))


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

(provide 'test-mevedel-system)
;;; test-mevedel-system.el ends here
