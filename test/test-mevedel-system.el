;;; test-mevedel-system.el --- Tests for mevedel-system.el -*- lexical-binding: t -*-

;;; Commentary:

;; Registration-only tests: the content of the system prompt is string
;; data, not logic.  These tests verify the builder assembles the
;; expected sections and loads workspace configuration files.

;;; Code:

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

  :doc "omits Workspace Configuration when neither file exists"
  (let* ((ws (mevedel-workspace-get-or-create
              'project root-dir root-dir "sysproj"))
         (prompt (mevedel-system-build-prompt "BASE" ws)))
    (should-not (string-match-p "## Workspace Configuration" prompt))))


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
