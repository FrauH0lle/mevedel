;; -*- mode: eask; lexical-binding: t -*-

(package "mevedel"
         "0.5.0"
         "Instructed LLM programmer/assistant")

(website-url "https://github.com/FrauH0lle/mevedel")
(keywords "convenience" "tools" "llm" "gptel" "gptel-agent")

(package-file "mevedel.el")
(files :defaults
       '("agents" "agents/*.md")
       '("prompts/context-summary" "prompts/context-summary/*.md")
       '("prompts/init" "prompts/init/*.md")
       '("prompts/permissions" "prompts/permissions/*.md")
       '("prompts/system" "prompts/system/*.md")
       '("skills/analyze-log" "skills/analyze-log/SKILL.md")
       '("skills/remember" "skills/remember/SKILL.md")
       '("skills/review" "skills/review/SKILL.md")
       '("collaboration" "collaboration/*")
       '("docs" "docs/*.md")
       '("docs/adr" "docs/adr/*.md")
       '("docs/agents" "docs/agents/*.md")
       '("prompts/tools" "prompts/tools/*.md"))

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "31.1")
(depends-on "gptel")
(depends-on "gptel-agent")
(depends-on "websocket")
(depends-on "qrencode")

(development
 (depends-on "mcp"))

(defun mevedel-eask--isolate-test-home ()
  "Run ERT without access to the user's real home state."
  (let ((home (make-temp-file "mevedel-test-home-" t)))
    (setq default-directory (expand-file-name default-directory))
    (setenv "HOME" home)
    ;; Emacs cached the real home the first time anything abbreviated a file
    ;; name during startup.  The cache guards itself by remembering which home
    ;; it was built from, so a stale regexp is inert -- until a test rebinds
    ;; `abbreviated-home-dir' and the recomputation refreshes that guard
    ;; globally while the regexp itself reverts.  Recompute both here, from the
    ;; isolated home, so the pair can never disagree.
    (setq abbreviated-home-dir nil)
    (dolist (entry '(("XDG_CACHE_HOME" . ".cache")
                     ("XDG_CONFIG_HOME" . ".config")
                     ("XDG_DATA_HOME" . ".local/share")
                     ("XDG_STATE_HOME" . ".local/state")))
      (setenv (car entry) (file-name-concat home (cdr entry))))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (ignore-errors (delete-directory home t))))))

(add-hook 'eask-before-test/ert-hook #'mevedel-eask--isolate-test-home)
