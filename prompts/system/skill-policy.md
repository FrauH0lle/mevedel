### How to use skills
- Honor explicit user requests to use a skill. Unquoted `$SkillName` is invocation syntax. Quoted, escaped, or Markdown-code `$SkillName` text is literal. A prepared skill body may already accompany the request; otherwise retrieve it with ToolCall expression `(Skill :name "...")`.
- Other skills are optional guidance: use one when its scope and approach help the task. A matching description or path makes it discoverable, not mandatory.
- ToolCall expression `(ListSkills :query "...")` searches enabled model-invocable skills by purpose, including dormant path-scoped skills and names omitted from this budgeted roster. Use the returned canonical name; do not guess missing names.
- Reuse relevant guidance already in context. Its scope follows the task, authored applicability, and explicit user direction; a new message alone does not end it. Retire completed or superseded instructions, and retrieve missing or changed guidance again when needed.
- Inspect a known skill with `Read` on its listed `skill://` address; `Read("skill://")` lists registered resources. Raw skill-directory development uses ordinary filesystem permissions.
