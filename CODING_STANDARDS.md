# Coding standards

Apply these rules to the changed design, code, tests, and documentation in a
diff. Cite the applicable rule and concrete effect when reporting a violation.
Use judgement for design tradeoffs; distinguish them from deterministic
requirements and use existing check results for requirements tooling enforces.

## Development conventions

[docs/development.md](docs/development.md) is the source of truth for code and
test conventions and required verification. Apply its relevant sections rather
than maintaining a second rule set here:

- [Code style](docs/development.md#code-style): source layout, declarations,
  naming, paths, strings, and diagnostics.
- [Loading and initialization](docs/development.md#loading-and-initialization):
  feature-boundary ownership, eager/deferred dependencies, cold first-use
  correctness, and startup/first-use/steady-state tradeoffs.
- [Testing conventions](docs/development.md#testing-conventions): coverage for
  new functions and changed behavior, real fixtures, isolation, silent output,
  and composer preservation during redraws.
- [Development commands](docs/development.md#development-commands): isolated
  Eask execution, pre-test bytecode cleanup, and warning-free compilation
  before committing.
- [gptel source rule](docs/development.md#gptel-source-rule): upstream API and
  pattern reuse for changes at the gptel boundary.

Before reviewing `relay/` or `shared-editing/`, also read
[relay/AGENTS.md](relay/AGENTS.md) or
[shared-editing/AGENTS.md](shared-editing/AGENTS.md), respectively, for their
scoped contracts and checks. Apply area-specific contracts from the
[documentation map](docs/index.md#documentation-map) to the changed area.

The [deterministic standards check](docs/development.md#deterministic-standards-and-checkdoc-ratchet)
enforces Lisp binding headers/final footers, local `docs/` link targets, ADR
index integrity, and the changed-file checkdoc ratchet. Use its result instead
of manually rechecking those invariants; the guide states their exact scope.

## DEEP HARNESS

Design mevedel as a **deep module**: substantial useful behavior behind a small,
clear interface. The LLM is its caller. Depth means capability per amount the
caller must learn, not implementation size or the number of tools.

The interface includes everything the model must know to operate correctly:
tools, prompts, reminders, resource conventions, ordering constraints, and
recovery procedures. Moving required knowledge into a manual does not remove
it from the interface.

- Leave task strategy and ordinary reasoning to capable models. Supply user
  intent, relevant evidence, clear constraints, and acceptance criteria.
  Preserve explicitly requested workflows without imposing them on every task.
- Absorb execution mechanics and bookkeeping in the harness: permissions,
  execution targets, process and agent lifecycles, mutation tracking, and
  persistence. Keep consequential effects, failures, and limits visible.
- Expose clear, composable capabilities. Fewer tools help only when they reduce
  what the model must learn and coordinate; a generic tool with a complicated
  language can enlarge the interface.
- Prefer locality: an implementation fix should stay inside its owning module,
  without requiring new operating instructions across callers. Test behavior
  through the same interface callers use.
- Apply the deletion test to instructions and orchestration: if removing one
  loses nothing useful, delete it. If removing a module makes callers recreate
  its mechanics, it earns its place. Do not move obsolete prompt rituals into
  hardcoded controllers.
- Reassess interventions as models improve. Judge simplification by completed
  work, correctness, cost, latency, and user interruptions; fewer tokens alone
  do not establish a better interface.

## NO BACKWARDS COMPATIBILITY

mevedel is under active development and has no backwards-compatibility
contract. There is currently one known user, so prefer the cleanest current
design even when it breaks existing APIs, commands, configuration, persisted
state, or workflows.

- Do not add compatibility wrappers, aliases, shims, deprecation layers,
  dual-format readers/writers, or runtime version gates unless the user
  explicitly requests compatibility for that specific change. Standalone
  session migration is the exception below, not a runtime compatibility path.
- Remove superseded code and update all in-repo callers, tests, fixtures, and
  documentation in the same change. Do not leave the old path alongside the
  new one.
- Existing compatibility code is not precedent. Delete it when a touched
  design no longer needs it.
- Prefer a direct breaking change over complexity introduced solely to support
  older mevedel versions or previously persisted local state.
- Call out destructive effects in the handoff, but do not preserve the old
  behavior merely to avoid a break.

Maintain one standalone session migration script outside the runtime loader:

- Accept a session directory or project directory through one command-line
  entry point. Discover the relevant sessions and their stored versions and
  migrate them to the latest format without interactive prompts or requiring
  callers to select versions or invoke individual steps. Already-current
  sessions are a no-op; failures produce clear diagnostics and a nonzero exit
  status for automation.
- Support multiple source versions through an ordered chain of explicit
  version-to-next-version steps, applied oldest to newest from each session's
  stored version. Keep each step self-contained so support for the oldest
  versions can be dropped by deleting a prefix of the chain and raising the
  minimum supported version, without rewriting retained steps. Reject versions
  outside the supported range rather than guessing.
- When changing the persisted session format, update the script, its tests,
  and migration documentation in the same change. Keep legacy-format knowledge
  in the migration script, not in runtime readers or writers.

## ADRS ARE REVISABLE

An accepted ADR records why a decision was made at the time. It is not a
boundary on later work. When evidence changes the trade, change the ADR.

- Amend an ADR in the same change that changes the behavior it describes. An
  ADR documenting a design the code no longer has is worse than no ADR.
- Keep one canonical ADR per coherent decision. Fold amendments into its
  current explanation; when a choice is reversed, retain the previous choice,
  replacement, and reason in a clearly marked decision-history section.
  Consolidated records preserve original ADR IDs and their destinations in
  `docs/adr/README.md` (ID/destination integrity is enforced by
  `scripts/check-standards.py`). Keep independent decisions separate.
- State what moved the decision — a measurement, a failure, a constraint that
  turned out not to hold. "We changed our minds" is not a reason; "the profile
  put this at 21% and the check was redundant with the target-side proof" is.
- Do not defer to an ADR you disagree with. Argue with it, in it.

## Maintained documentation

The `docs/` tree documents the system as it exists now: implemented behavior,
current contracts, and the rationale for the current design. It is not a
planning workspace. `docs/backlog.md` is the sole future-work exception, holding
concise actionable entries. Keep detailed plans, PRDs, proposals, roadmaps,
reviews, progress reports, and speculative designs outside `docs/`; use the
[working-material policy](AGENTS.md#working-material) for their destinations.

Update `docs/` when the corresponding change is implemented; do not document
intended behavior as current behavior. Clearly marked ADR decision histories
retain historical rationale, not future plans.
