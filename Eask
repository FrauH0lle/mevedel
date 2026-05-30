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
       '("skills/coordinator" "skills/coordinator/SKILL.md")
       '("skills/remember" "skills/remember/SKILL.md")
       '("skills/review" "skills/review/SKILL.md")
       '("tools" "tools/*.md"))

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "30.1")
(depends-on "gptel"
            :fetcher 'github
            :repo "karthink/gptel")
(depends-on "gptel-agent"
            :fetcher 'github
            :repo "karthink/gptel-agent")

(development
 (depends-on "mcp"
             :fetcher 'github
             :repo "lizqwerscott/mcp.el"))
