---
name: write-a-skill
description: Create new agent skills with proper structure, progressive disclosure, and bundled resources. Use when user wants to create, write, or build a new skill.
---

# Writing Skills

## Process

1. **Gather requirements** - ask user about:
   - What task/domain does the skill cover?
   - What specific use cases should it handle?
   - Does it need executable scripts or just instructions?
   - Any reference materials to include?
   - Should it run inline in the current request, or in a forked agent context?
   - Should the user invoke it directly, should the model invoke it, or both?
   - Does it need temporary permission grants, model overrides, path gating, or hooks?

2. **Draft the skill** - create:
   - SKILL.md with concise instructions
   - Additional reference files if content exceeds 500 lines
   - Utility scripts if deterministic operations needed

3. **Review with user** - present draft and ask:
   - Does this cover your use cases?
   - Anything missing or unclear?
   - Should any section be more/less detailed?

## Skill Structure

```
skill-name/
├── SKILL.md           # Main instructions (required)
├── REFERENCE.md       # Detailed docs (if needed)
├── EXAMPLES.md        # Usage examples (if needed)
└── scripts/           # Utility scripts (if needed)
    └── helper.js
```

## SKILL.md Template

```md
---
name: skill-name
description: Brief description of capability. Use when [specific triggers].
context: inline
---

# Skill Name

## Quick start

[Minimal working example]

## Workflows

[Step-by-step processes with checklists for complex tasks]

## Advanced features

[Link to separate files: See [REFERENCE.md](REFERENCE.md)]
```

## Mevedel Frontmatter

Mevedel supports Claude Code style skills plus mevedel-specific execution
fields. Use only the fields that change behavior; omit defaults.

Common fields:

- `name` - invocation identifier. Must be lowercase kebab-case
  (`[a-z0-9-]+`) and no longer than 64 characters. If omitted, the directory
  name is used.
- `display-name` - human-friendly label for completion/listing UI.
- `description` - model-visible summary. Include concrete "Use when..."
  triggers.
- `when_to_use` / `when-to-use` - optional longer trigger guidance.
- `argument-hint` - completion hint for slash arguments.
- `arguments` - named positional arguments used for `$name` substitution.

Invocation gates:

- `user-invocable: false` hides the skill from slash completion and blocks
  `/skill`.
- `disable-model-invocation: true` keeps the skill off the model-visible Skill
  listing and blocks model-side `Skill(name=...)`.

Execution fields:

- `context: inline` expands the skill body into the current request. This is
  the default.
- `context: fork` runs the prepared body in a foreground sub-agent.
- `agent: explorer|coordinator|verifier|reviewer` selects a registered
  mevedel agent for forked skills. If omitted, mevedel synthesizes a skill
  agent that inherits the parent context.
- `allowed-tools` adds temporary permission grants while the skill is active.
  It does not remove tools and does not deny unspecified tools. Examples:
  `Bash(git status *)`, `Agent(explorer)`, `Eval`.
- `model` temporarily overrides the active model/tier for the skill request.
- `effort` accepts `low`, `medium`, `high`, `xhigh`, or `max`; it is parsed but
  currently inert until the backend exposes a reasoning-effort knob.
- `paths` is a list of globs that gates model-listing visibility only.
  Explicit slash/model invocation by name can still run the skill.
- `shell: bash` is the default for body shell injections. `powershell` parses
  but is not generally useful in this repo.
- `hooks` installs skill-scoped mevedel hooks for the invocation. Use the same
  event -> matcher -> handler shape as `.mevedel/hooks.el` / `.mevedel/hooks.json`.

Body features:

- `$ARGUMENTS`, `$ARGUMENTS[0]`, `$0`, `$1`, and named `$argument` placeholders
  are substituted before invocation.
- `${CLAUDE_SKILL_DIR}` expands to the skill directory and is useful for
  executable scripts or explicit companion-file paths.
- Shell injections (`` !`cmd` `` and fenced ```! blocks) require matching
  `allowed-tools` Bash grants.
- Elisp injections (`` !el`...` `` and fenced ```!el blocks) require
  `allowed-tools: [Eval]`.

Relative Markdown links to companion files are fine as authoring references.
They are not automatically expanded by mevedel, Claude Code, or Codex; if the
model must read a companion file during execution, tell it to read the file or
reference `${CLAUDE_SKILL_DIR}/FILE.md`.

## Description Requirements

The description is **the only thing your agent sees** when deciding which skill to load. It's surfaced in the system prompt alongside all other installed skills. Your agent reads these descriptions and picks the relevant skill based on the user's request.

**Goal**: Give your agent just enough info to know:

1. What capability this skill provides
2. When/why to trigger it (specific keywords, contexts, file types)

**Format**:

- Max 1024 chars
- Write in third person
- First sentence: what it does
- Second sentence: "Use when [specific triggers]"

**Good example**:

```
Extract text and tables from PDF files, fill forms, merge documents. Use when working with PDF files or when user mentions PDFs, forms, or document extraction.
```

**Bad example**:

```
Helps with documents.
```

The bad example gives your agent no way to distinguish this from other document skills.

## When to Add Scripts

Add utility scripts when:

- Operation is deterministic (validation, formatting)
- Same code would be generated repeatedly
- Errors need explicit handling

Scripts save tokens and improve reliability vs generated code.

## When to Split Files

Split into separate files when:

- SKILL.md exceeds 100 lines
- Content has distinct domains (finance vs sales schemas)
- Advanced features are rarely needed

## Review Checklist

After drafting, verify:

- [ ] Description includes triggers ("Use when...")
- [ ] SKILL.md under 100 lines
- [ ] No time-sensitive info
- [ ] Consistent terminology
- [ ] Concrete examples included
- [ ] References one level deep
- [ ] Mevedel frontmatter defaults are omitted unless intentionally changed
- [ ] `allowed-tools` entries name registered mevedel tools and valid qualifiers
