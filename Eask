;; -*- mode: eask; lexical-binding: t -*-

(package "mevedel"
         "0.5.0"
         "Instructed LLM programmer/assistant")

(website-url "https://github.com/FrauH0lle/mevedel")
(keywords "convenience" "tools" "llm" "gptel" "gptel-agent")

(package-file "mevedel.el")
(files :defaults
       '("agents" "agents/*.md")
       '("prompts/compaction" "prompts/compaction/*.md")
       '("prompts/init" "prompts/init/*.md")
       '("prompts/permissions" "prompts/permissions/*.md")
       '("prompts/system" "prompts/system/*.md")
       '("skills/analyze-log" "skills/analyze-log/SKILL.md")
       '("skills/remember" "skills/remember/SKILL.md")
       '("skills/review" "skills/review/SKILL.md")
       '("tools" "tools/*.md"))

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "30.2")
(depends-on "gptel")
(depends-on "gptel-agent")

(development
 (depends-on "mcp"))

(defun mevedel-eask--isolate-test-home ()
  "Run ERT without access to the user's real home state."
  (let ((home (make-temp-file "mevedel-test-home-" t)))
    (setq default-directory (expand-file-name default-directory))
    (setenv "HOME" home)
    (dolist (entry '(("XDG_CACHE_HOME" . ".cache")
                     ("XDG_CONFIG_HOME" . ".config")
                     ("XDG_DATA_HOME" . ".local/share")
                     ("XDG_STATE_HOME" . ".local/state")))
      (setenv (car entry) (file-name-concat home (cdr entry))))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (ignore-errors (delete-directory home t))))))

(add-hook 'eask-before-test/ert-hook #'mevedel-eask--isolate-test-home)
