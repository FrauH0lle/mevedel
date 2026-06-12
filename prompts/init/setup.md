Set up mevedel-native repository guidance for this workspace.

You are running inside mevedel. Your job is to inspect the repository,
ask only the useful setup questions, and then create or improve the
files that make future agent work in this workspace sharper.

Workspace root: {{WORKSPACE_ROOT}}
Working directory: {{WORKING_DIRECTORY}}
Project target: {{TARGET_FILE}}
Personal target: {{LOCAL_TARGET_FILE}}
Project skills: {{PROJECT_SKILLS_DIR}}
User skills: {{USER_SKILLS_DIR}}
Hooks file: {{HOOKS_FILE}}
Hooks scripts: {{HOOKS_DIR}}
User-provided focus: {{ARGUMENTS}}

## Outcome

Produce high-signal, repo-specific guidance. Prefer a small accurate
artifact over a comprehensive but generic one. Reuse useful material
from existing AI instruction files, but convert it to mevedel-native
shape when writing new files.

## Phase 1: Choose Scope

Use `Ask` before making edits. Ask what to set up and include compact
previews when a choice would materially change the files.

Good first questions:

- Instruction target:
  - `{ label: "Project AGENTS.md (Recommended)", description: "Shared repo guidance committed with the project." }`
  - `{ label: "Personal AGENTS.local.md", description: "Private local guidance for this checkout." }`
  - `{ label: "Both project + personal", description: "Shared guidance plus private preferences." }`
- Optional mevedel artifacts:
  - `{ label: "Skills + hooks (Recommended)", description: "Add reusable workflows and lifecycle automation when the repo justifies them." }`
  - `{ label: "Skills only", description: "Add slash-invocable workflows without hooks." }`
  - `{ label: "Hooks only", description: "Add lifecycle automation without skills." }`
  - `{ label: "Neither, just instructions", description: "Only write instruction files." }`

If `{{ARGUMENTS}}` is non-empty, incorporate that focus into the first
question or skip any question it clearly answers.

## Phase 2: Explore

Inspect before writing. For a non-trivial repository, dispatch an
`Agent` to survey structure, commands, tests, conventions, and existing
agent instructions while you inspect the files most likely to be
authoritative.

Prioritize:

- `README*`, package manifests, build files, lockfiles, workspace config
- test, lint, format, typecheck, codegen, CI, pre-commit, release config
- `AGENTS.md`, `AGENTS.local.md`, `CLAUDE.md`, `.cursor/rules/`,
  `.cursorrules`, `.github/copilot-instructions.md`, `.windsurfrules`,
  `.clinerules`, `opencode.json`, `.mcp.json`
- existing `.mevedel/skills/`, `.mevedel/hooks.json`,
  `.mevedel/hooks/`, `.claude/skills/`
- `git worktree list`, when sibling worktrees may carry fresher local
  guidance

Prefer executable truth over prose. Validate commands from manifests or
CI instead of copying stale README instructions blindly.

## Phase 3: Fill Gaps

Use `Ask` only when the repository cannot answer an important question:
preferred package manager, expensive test commands, private workflow
preferences, local-only secrets or paths, or whether to commit project
hooks.

Do not ask for permission to continue after each phase. Make reasonable
choices and explain them in the final response.

## Phase 4: Propose

Before writing, summarize the intended files and use `Ask` with object
options. Include short `preview` text when it helps the user compare
choices. Keep previews compact; they are for decision support, not a
full draft.

Example options:

- `{ label: "Looks good (Recommended)", description: "Write the proposed setup.", preview: "AGENTS.md with commands, style, tests; one review skill; no hooks." }`
- `{ label: "Drop skills", description: "Write instruction files only plus any approved hooks." }`
- `{ label: "Drop hooks", description: "Write instructions and skills, but no hook config." }`
- `{ label: "Instructions only", description: "Only write AGENTS.md or AGENTS.local.md." }`

## Phase 5: Write Instructions

When creating a new shared file, use `# Repository Guidelines` unless
the repo already uses a better title. When improving an existing file,
preserve accurate project-specific guidance and remove generic filler.

Good content:

- exact build, test, lint, format, typecheck, codegen, and dev commands
- focused test commands for the changed area
- architecture landmarks and ownership boundaries
- style rules that differ from defaults or are enforced by tooling
- testing quirks, environment setup, generated files, and release traps
- external AI rules that still apply and are worth preserving

Avoid:

- generic agent etiquette or obvious language/framework conventions
- exhaustive file trees
- stale commands copied without checking likely sources
- speculative process rules
- long documentation dumps

Fallback headings when a new small file needs shape:

- Project Structure
- Build/Test/Dev Commands
- Style
- Testing
- Architecture
- Agent Notes
- Security/Config

For `AGENTS.local.md`, keep private preferences, local paths, and
checkout-specific workflow notes out of the shared file. If it is added
to a repository where it should remain private, add or update a
`.gitignore` entry for `AGENTS.local.md`.

## Phase 6: Write Skills

Create skills only when they capture a recurring workflow better than
plain instructions. Use project skills under `{{PROJECT_SKILLS_DIR}}`
for repo-specific workflows and user skills under `{{USER_SKILLS_DIR}}`
only for reusable personal workflows.

Each skill should have a `SKILL.md` with concise frontmatter and a body
that tells the model when to use the workflow, what to inspect, what to
produce, and which checks matter. Use `disable-model-invocation: true`
when a skill is meant for explicit slash use only.

## Phase 7: Write Hooks

Create hooks only when automation has a clear payoff. Prefer simple
project hooks in `{{HOOKS_FILE}}` plus scripts in `{{HOOKS_DIR}}`.
Make hook behavior inspectable and bounded. If adding project hooks,
tell the user they must review and trust them with
`M-x mevedel-hooks-trust-project`.

## Phase 8: Final Response

Report:

- files created or changed
- key guidance captured
- skills or hooks added and why
- commands you verified, or why you did not run them
- any follow-up the user should review manually

Do not claim hooks are trusted or active unless that is actually true.
Do not bypass normal tool permissions.
