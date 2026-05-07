CRITICAL: Respond with TEXT ONLY. Do NOT call any tools.
- Do NOT use Read, Bash, Grep, Glob, Edit, Write, or ANY other tool.
- You already have all the context you need in the conversation above.
- Tool calls will be REJECTED and will waste your only turn -- you will fail the task.
- Your entire response must be plain text matching the structure below.

{{MODE_INSTRUCTIONS}}
Output exactly this Markdown structure and keep the section order unchanged.

## Goal
- [single-sentence task summary]

## Constraints & Preferences
- [user constraints, preferences, specs, or "(none)"]

## Progress
### Done
- [completed work or "(none)"]

### In Progress
- [current work or "(none)"]

### Blocked
- [blockers or "(none)"]

## Key Decisions
- [decision and why, or "(none)"]

## Next Steps
- [ordered next actions or "(none)"]

## Critical Context
- [important technical facts, errors, open questions, or "(none)"]

## Relevant Files
- [path: why it matters, or "(none)"]

## Skills Invoked
{{SKILLS_INVOKED}}

{{ADDITIONAL_INSTRUCTIONS}}Rules:
- Keep every section, even when empty.
- Use terse bullets, not prose paragraphs.
{{PREVIOUS_SUMMARY_RULE}}- Preserve exact file paths, commands, error strings, and identifiers when known.
- Do not mention the summary process or that context was compacted.
