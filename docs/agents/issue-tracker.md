# Issue tracker: Local Markdown

Issues and PRDs for this repo live as markdown files in `.scratch/`.
That directory is intentionally gitignored local agent state. Keep plans,
proposals, roadmaps, review reports, and speculative designs there, outside
`docs/`. [The backlog](../backlog.md) holds concise actionable future-work
entries; detailed work belongs in the corresponding scratch directory.
The rest of `docs/` describes the current system. Once a PRD's product or
architecture decisions are implemented, record the resulting behavior and
rationale in `docs/adr/` or the relevant area doc instead of committing
`.scratch/`.


## Conventions

- One feature per directory: `.scratch/<feature-slug>/`
- The PRD is `.scratch/<feature-slug>/PRD.md`
- Implementation issues are `.scratch/<feature-slug>/issues/<NN>-<slug>.md`, numbered from `01`
- Triage state is recorded as a `Status:` line near the top of each issue file (see [triage labels](triage-labels.md) for the role strings)
- Comments and conversation history append to the bottom of the file under a `## Comments` heading
- Do not commit `.scratch/` files; they are planning and coordination
  state, not maintained project documentation

## When a skill says "publish to the issue tracker"

Create a new file under `.scratch/<feature-slug>/` (creating the directory if needed).

## When a skill says "fetch the relevant ticket"

Read the file at the referenced path. The user will normally pass the path or the issue number directly.

## Wayfinding operations

When explicitly using the `wayfinder` skill, its map and decision tickets use
this local tracker. This convention does not require a map for ordinary work.

- **Map**: `.scratch/<effort>/map.md` — the Destination, Notes, Decisions so far, Not yet specified, and Out of scope sections.
- **Child ticket**: `.scratch/<effort>/issues/NN-<slug>.md`, numbered from `01`, with the question in the body. A `Type:` line records the ticket type (`research`/`prototype`/`grilling`/`task`); a `Status:` line records `claimed`/`resolved`.
- **Blocking**: a `Blocked by: NN, NN` line near the top. A ticket is unblocked when every file it lists is `resolved`.
- **Frontier**: scan `.scratch/<effort>/issues/` for files that are open, unblocked, and unclaimed; first by number wins.
- **Claim**: set `Status: claimed` and save before any work.
- **Resolve**: append the answer under an `## Answer` heading, set `Status: resolved`, then append a context pointer (gist + link) to the map's Decisions-so-far in `map.md`.
