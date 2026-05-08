;;; test-mevedel-system.el --- Tests for mevedel-system.el -*- lexical-binding: t -*-

;;; Commentary:

;; Registration-only tests: the content of the system prompt is string
;; data, not logic.  These tests verify the builder assembles the
;; expected sections and loads workspace configuration files.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
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
    (should (string-match-p "Critical thinking" mevedel-system--tone-prompt))))


;;
;;; Base prompt (string constant)

(mevedel-deftest mevedel-system--base-prompt
  (:doc "`mevedel-system--base-prompt' includes task protocol and tool guidance")
  (progn
    (should (stringp mevedel-system--base-prompt))
    (should (string-match-p "Tone and style" mevedel-system--base-prompt))
    (should (string-match-p "Task execution protocol" mevedel-system--base-prompt))
    (should (string-match-p "Using your tools" mevedel-system--base-prompt))
    (should (string-match-p "explorer" mevedel-system--base-prompt))
    (should (string-match-p "planner" mevedel-system--base-prompt))))


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
                      (make-temp-file "mevedel-sys-" t))))
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

  :doc "includes CLAUDE.md content when AGENTS.md is absent"
  (let* ((claude-md (file-name-concat root-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "Claude-specific guidance." nil claude-md)
    (let ((prompt (mevedel-system-build-prompt "BASE" ws)))
      (should (string-match-p "## Workspace Configuration" prompt))
      (should (string-match-p "Claude-specific guidance" prompt))))

  :doc "prefers AGENTS.md when both files exist"
  (let* ((agents-md (file-name-concat root-dir "AGENTS.md"))
         (claude-md (file-name-concat root-dir "CLAUDE.md"))
         (ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj")))
    (write-region "AGENTS wins." nil agents-md)
    (write-region "CLAUDE loses." nil claude-md)
    (let ((prompt (mevedel-system-build-prompt "BASE" ws)))
      (should (string-match-p "AGENTS wins" prompt))
      (should-not (string-match-p "CLAUDE loses" prompt))))

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

  :doc "prefers AGENTS.md over CLAUDE.md in each layered directory"
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
      (should (string-match-p "Root Claude guidance\\." prompt))
      (should (string-match-p "Module AGENTS guidance\\." prompt))
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
                      (make-temp-file "mevedel-agent-sys-" t))))
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
      (should-not (string-match-p "Remembered fact" prompt)))))


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
                      (make-temp-file "mevedel-memory-" t))))
   :after-each (progn
                 (mevedel-workspace-clear-registry)
                 (delete-directory root-dir t)))
  ,test
  (test)
  :doc "returns empty index guidance when MEMORY.md is absent"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (content (mevedel-system--memory-content ws)))
    (should (string-match-p "MEMORY.md index is currently empty" content))
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
               "\\`<!-- Last updated: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} (today) -->"
               content))
      (should (string-match-p "line-001" content))
      (should (string-match-p "line-200" content))
      (should-not (string-match-p "line-201" content)))))

(mevedel-deftest mevedel-system--memory-cache-key
  (:before-each (mevedel-workspace-clear-registry)
   :vars* ((root-dir (file-name-as-directory
                      (make-temp-file "mevedel-memory-cache-" t))))
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
  :doc "renders registered sections in order"
  (let ((mevedel-system--prompt-sections nil)
        (mevedel-system--prompt-section-cache (make-hash-table :test #'equal))
        (ws (mevedel-workspace-get-or-create
             'project root-dir root-dir "sysproj")))
    (mevedel-define-prompt-section later
      :order 20
      :producer (lambda (_context) "later"))
    (mevedel-define-prompt-section earlier
      :order 10
      :producer (lambda (_context) "earlier"))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("earlier" "later"))))

  :doc "memoizes keyed sections until their cache key changes"
  (let ((mevedel-system--prompt-sections nil)
        (mevedel-system--prompt-section-cache (make-hash-table :test #'equal))
        (cache-key 'same)
        (calls 0)
        (ws (mevedel-workspace-get-or-create
             'project root-dir root-dir "sysproj")))
    (mevedel-define-prompt-section cached
      :order 10
      :cache 'keyed
      :cache-key (lambda (_context) cache-key)
      :producer (lambda (_context)
                  (setq calls (1+ calls))
                  (format "call-%d" calls)))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-1")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-1")))
    (should (= calls 1))
    (setq cache-key 'changed)
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("call-2")))
    (should (= calls 2)))

  :doc "invalidates cached section values when a section is re-registered"
  (let ((mevedel-system--prompt-sections nil)
        (mevedel-system--prompt-section-cache (make-hash-table :test #'equal))
        (old-calls 0)
        (new-calls 0)
        (ws (mevedel-workspace-get-or-create
             'project root-dir root-dir "sysproj")))
    (mevedel-define-prompt-section reloadable
      :order 10
      :cache 'keyed
      :cache-key (lambda (_context) 'same)
      :producer (lambda (_context)
                  (setq old-calls (1+ old-calls))
                  "old"))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("old")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("old")))
    (should (= old-calls 1))
    (mevedel-define-prompt-section reloadable
      :order 10
      :cache 'keyed
      :cache-key (lambda (_context) 'same)
      :producer (lambda (_context)
                  (setq new-calls (1+ new-calls))
                  "new"))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("new")))
    (should (equal (mevedel-system-render-sections "BASE" ws)
                   '("new")))
    (should (= old-calls 1))
    (should (= new-calls 1)))

  :doc "recomputes uncached sections on each render"
  (let ((mevedel-system--prompt-sections nil)
        (mevedel-system--prompt-section-cache (make-hash-table :test #'equal))
        (calls 0)
        (ws (mevedel-workspace-get-or-create
             'project root-dir root-dir "sysproj")))
    (mevedel-define-prompt-section uncached
      :order 10
      :producer (lambda (_context)
                  (setq calls (1+ calls))
                  (format "call-%d" calls)))
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
