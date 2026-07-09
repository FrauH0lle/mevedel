;;; test-mevedel-system.el --- Tests for mevedel-system.el -*- lexical-binding: t -*-

;;; Commentary:

;; Registration-only tests: the content of the system prompt is string
;; data, not logic.  These tests verify the builder assembles the
;; expected sections and loads workspace configuration files.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'mevedel-skills)
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


;;
;;; Tone prompt (string constant)

(mevedel-deftest mevedel-system--tone-prompt
  (:doc "`mevedel-system--tone-prompt' includes tone and critical-thinking sections")
  (progn
    (should (stringp mevedel-system--tone-prompt))
    (should (string-match-p "Tone and style" mevedel-system--tone-prompt))
    (should (string-match-p "Critical thinking" mevedel-system--tone-prompt))
    (should (string-match-p "em dashes" mevedel-system--tone-prompt))
    (should (string-match-p "fenced code blocks" mevedel-system--tone-prompt))
    (should (string-match-p "Markdown structure" mevedel-system--tone-prompt))
    (should (string-match-p "repeating the path noisily" mevedel-system--tone-prompt))))


;;
;;; Base prompt (string constant)

(mevedel-deftest mevedel-system--base-prompt
  (:doc "`mevedel-system--base-prompt' includes task protocol and tool guidance")
  (progn
    (should (stringp mevedel-system--base-prompt))
    (should (string-match-p "Tone and style" mevedel-system--base-prompt))
    (should (string-match-p "Task execution protocol" mevedel-system--base-prompt))
    (should (string-match-p "Using your tools" mevedel-system--base-prompt))
    (should (string-match-p "path/to/file\\.ext:123" mevedel-system--base-prompt))
    (should (string-match-p "update item statuses" mevedel-system--base-prompt))
    (should (string-match-p "Frontend work" mevedel-system--base-prompt))
    (should (string-match-p "explorer" mevedel-system--base-prompt))
    (should (string-match-p "/plan" mevedel-system--base-prompt))))


;;
;;; Tutor prompt (string constant)

(mevedel-deftest mevedel-system--tutor-base-prompt
  (:doc "`mevedel-system--tutor-base-prompt' requires GetHints/RecordHint workflow")
  (progn
    (should (stringp mevedel-system--tutor-base-prompt))
    (should (string-match-p "NEVER PROVIDE SOLUTIONS" mevedel-system--tutor-base-prompt))
    (should (string-match-p "GetHints" mevedel-system--tutor-base-prompt))
    (should (string-match-p "RecordHint" mevedel-system--tutor-base-prompt))
    (should (string-match-p "Socratic" mevedel-system--tutor-base-prompt))))


;;
;;; Prompt builder

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
         (prompt (mevedel-system-build-prompt "BASE PROMPT CONTENT" ws)))
    (should (string-match-p "BASE PROMPT CONTENT" prompt))
    (should (string-match-p "Persistent memory" prompt))
    (should (string-match-p "## Environment" prompt))
    (should (string-match-p "Emacs version:" prompt))
    (should (string-match-p (regexp-quote emacs-version) prompt))
    (should (string-match-p "<env>" prompt)))

  :doc "includes AGENTS.md content when present"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Use bun, not npm." nil agents-md)
    (let ((prompt (mevedel-system-build-prompt "BASE" ws)))
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
    (let* ((prompt (mevedel-system-build-prompt "BASE" ws))
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
                      "BASE" ws nil session (current-buffer)))
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
                     "BASE" ws root-dir session (current-buffer))))
        (should-not (string-match-p "## Skills" prompt)))))

  :doc "ignores CLAUDE.md when AGENTS.md is absent"
  (let* ((claude-md (file-name-concat root-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Claude-specific guidance." nil claude-md)
    (let ((prompt (mevedel-system-build-prompt "BASE" ws)))
      (should-not (string-match-p "## Workspace Configuration" prompt))
      (should-not (string-match-p "Claude-specific guidance" prompt))))

  :doc "uses AGENTS.md and ignores CLAUDE.md when both files exist"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (claude-md (file-name-concat root-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "AGENTS wins." nil agents-md)
    (write-region "CLAUDE loses." nil claude-md)
    (let ((prompt (mevedel-system-build-prompt "BASE" ws)))
      (should (string-match-p "AGENTS wins" prompt))
      (should-not (string-match-p "CLAUDE loses" prompt))))

  :doc "loads AGENTS.local.md after the shared file in the same directory"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (local-md (file-name-concat root-dir "AGENTS.local.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Shared guidance." nil agents-md)
    (write-region "Private guidance." nil local-md)
    (let* ((prompt (mevedel-system-build-prompt "BASE" ws))
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
    (let* ((prompt (mevedel-system-build-prompt "BASE" ws module-dir))
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
    (let* ((prompt (mevedel-system-build-prompt "BASE" ws module-dir))
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
    (let ((prompt (mevedel-system-build-prompt "BASE" ws module-dir)))
      (should (string-match-p "Module AGENTS guidance\\." prompt))
      (should-not (string-match-p "Root Claude guidance\\." prompt))
      (should-not (string-match-p "Module Claude loses\\." prompt))))

  :doc "omits Workspace Configuration when neither file exists"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (prompt (mevedel-system-build-prompt "BASE" ws)))
    (should-not (string-match-p "## Workspace Configuration" prompt)))

  :doc "does not reuse a different base prompt from the section cache"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (_prompt-one (mevedel-system-build-prompt "BASE ONE" ws))
         (prompt-two (mevedel-system-build-prompt "BASE TWO" ws)))
    (should (string-match-p "BASE TWO" prompt-two))
    (should-not (string-match-p "BASE ONE" prompt-two))))

(mevedel-deftest mevedel-system-build-agent-prompt
  (:before-each (mevedel-workspace-clear-registry)
   :after-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-agent-sys-" t)))
           (mevedel-memory-dirs '(".mevedel/memory/")))
   :after-each (delete-directory root-dir t))
  ,test
  (test)
  :doc "can omit workspace configuration and memory while keeping environment"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (memory-dir (file-name-concat root-dir ".mevedel" "memory"))
         (memory-file (file-name-concat memory-dir "MEMORY.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (make-directory memory-dir t)
    (write-region "Workspace guidance." nil agents-md)
    (write-region "Remembered fact." nil memory-file)
    (let ((prompt (mevedel-system-build-agent-prompt
                   "AGENT BASE" :workspace ws
                   :workspace-config nil
                   :memory nil
                   :environment t)))
      (should (string-match-p "AGENT BASE" prompt))
      (should (string-match-p "## Environment" prompt))
      (should-not (string-match-p "Workspace guidance" prompt))
      (should-not (string-match-p "Remembered fact" prompt))))

  :doc "can include the parent session skills roster for skill-capable agents"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (session (mevedel-session-create "main" ws))
         (skill (mevedel-skill--create
                 :name "domain-modeling"
                 :description "Sharpen terminology"
                 :active-p t
                 :model-invocable-p t)))
    (setf (mevedel-session-skills session) (list skill))
    (with-temp-buffer
      (let ((prompt (mevedel-system-build-agent-prompt
                     "AGENT BASE" :workspace ws
                     :session session
                     :refresh-buffer (current-buffer)
                     :workspace-config nil
                     :memory nil
                     :environment nil
                     :skills t)))
        (should (string-match-p "AGENT BASE" prompt))
        (should (string-match-p "^- domain-modeling: Sharpen terminology$"
                                prompt))))
    (with-temp-buffer
      (let ((prompt (mevedel-system-build-agent-prompt
                     "AGENT BASE" :workspace ws
                     :session session
                     :workspace-config nil
                     :memory nil
                     :environment nil)))
        (should-not (string-match-p "## Skills" prompt))))))


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
                   :base-prompt "BASE"
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
;;; Prompt section registry

(mevedel-deftest mevedel-system-render-sections
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-section-" t))))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "renders fixed sections in declared order"
  (let ((mevedel-system--prompt-sections
         (list (list :name 'earlier
                     :producer (lambda (_context) "earlier"))
               (list :name 'later
                     :producer (lambda (_context) "later"))))
        (mevedel-system--prompt-section-cache (make-hash-table :test #'equal))
        (ws (mevedel-workspace-get-or-create
             'project root-dir root-dir "sysproj")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("earlier" "later"))))

  :doc "memoizes keyed sections until their cache key changes"
  (let* ((mevedel-system--prompt-section-cache
          (make-hash-table :test #'equal))
         (cache-key 'same)
         (calls 0)
         (mevedel-system--prompt-sections
          (list (list :name 'cached
                      :cache-key (lambda (_context) cache-key)
                      :producer (lambda (_context)
                                  (setq calls (1+ calls))
                                  (format "call-%d" calls)))))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-1")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-1")))
    (should (= calls 1))
    (setq cache-key 'changed)
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-2")))
    (should (= calls 2)))

  :doc "recomputes uncached sections on each render"
  (let* ((mevedel-system--prompt-section-cache
          (make-hash-table :test #'equal))
         (calls 0)
         (mevedel-system--prompt-sections
          (list (list :name 'uncached
                      :producer (lambda (_context)
                                  (setq calls (1+ calls))
                                  (format "call-%d" calls)))))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-1")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-2")))
    (should (= calls 2))))


;;
;;; Prompt templates

(mevedel-deftest mevedel-system-render-agent-prompt-file
  (:vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-agent-template-" t))))
   :after-each (delete-directory root-dir t))
  ,test
  (test)
  :doc "expands agent prompt templates through the shared renderer"
  (let ((mevedel-system--source-dir root-dir)
        (prompt-file (file-name-concat root-dir "agent.md")))
    (write-region "Agent start\n{{TONE_PROMPT}}\nAgent end\n" nil prompt-file)
    (let ((prompt (mevedel-system-render-agent-prompt-file
                   "agent.md" '(("TONE_PROMPT" . "Tone body")))))
      (should (string-match-p "Agent start" prompt))
      (should (string-match-p "Tone body" prompt))
      (should-not (string-match-p "{{TONE_PROMPT}}" prompt)))))

(provide 'test-mevedel-system)
;;; test-mevedel-system.el ends here
