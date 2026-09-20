# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for
interacting with LLMs during programming. It enables overlay-based
instruction management for AI-assisted development with direct gptel
integration.

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
  dual-format readers/writers, version gates, or migrations unless the user
  explicitly requests compatibility for that specific change.
- Remove superseded code and update all in-repo callers, tests, fixtures, and
  documentation in the same change. Do not leave the old path alongside the
  new one.
- Existing compatibility code is not precedent. Delete it when a touched
  design no longer needs it.
- Prefer a direct breaking change over complexity introduced solely to support
  older mevedel versions or previously persisted local state.
- Call out destructive effects in the handoff, but do not preserve the old
  behavior merely to avoid a break.

## ADRS ARE REVISABLE

An accepted ADR records why a decision was made at the time. It is not a
boundary on later work. When evidence changes the trade, change the ADR.

- Amend an ADR in the same change that changes the behavior it describes. An
  ADR documenting a design the code no longer has is worse than no ADR.
- Keep one canonical ADR per coherent decision. Fold amendments into its
  current explanation; when a choice is reversed, retain the previous choice,
  replacement, and reason in a clearly marked decision-history section.
  Consolidated records preserve original ADR IDs and their destinations in
  `docs/adr/README.md`. Keep independent decisions separate.
- State what moved the decision — a measurement, a failure, a constraint that
  turned out not to hold. "We changed our minds" is not a reason; "the profile
  put this at 21% and the check was redundant with the target-side proof" is.
- Do not defer to an ADR you disagree with. Argue with it, in it.

## Documentation map

Before planning or changing an unfamiliar area, consult the
[documentation map](docs/index.md#documentation-map) and read its relevant
contracts. The `docs/` tree documents the system as it exists now: implemented
behavior, current contracts, and the rationale for the current design. It is
not a planning workspace. `docs/backlog.md` is the sole future-work exception,
holding concise actionable entries. Keep detailed plans, PRDs, proposals,
roadmaps, reviews, progress reports, and speculative designs outside `docs/`,
under `.scratch/<feature-slug>/`.
Update `docs/` when the corresponding change is implemented; do not document
intended behavior as current behavior. Clearly marked ADR decision histories
retain historical rationale, not future plans. This file keeps the entry rules and
retrieval triggers.

Each `.el` file also describes its purpose in its `;;; Commentary:` block.

Before working in `relay/` or `shared-editing/`, read
[relay/AGENTS.md](relay/AGENTS.md) or
[shared-editing/AGENTS.md](shared-editing/AGENTS.md), respectively, for
scoped contracts and checks.

## Module reference

Read [`docs/module-map.md`](docs/module-map.md) when locating a module or
choosing where a change belongs. It maps the entry point, data model, views,
prompts, agents, tools, and support modules. Area-specific design and behavioral
contracts remain in the documentation map above.

## Development contract

Before planning or implementing code changes, writing or running tests,
compiling, or committing, read [docs/development.md](docs/development.md).
It owns dependency setup, upstream checkout procedures, code conventions,
test structure, diagnostic handling, and compilation commands.

- New functions require tests; changed behavior requires updated tests.
- Run tests through the isolated Eask environment. Tests must leave no state
  behind and produce only expected ERT output.
- Run `npx @emacs-eask/cli clean elc` before tests, and compile without warnings
  before committing.
- Before changing prompts, requests, callbacks, tool calls, presets, buffers,
  transcripts, session flow, agents, or coordination, consult gptel
  source and reuse its APIs or patterns. Follow the development
  guide's upstream-source procedure.

For interactive commands, see [README.md](README.md#usage).

## Agent skills

### Issue tracker

Issues and PRDs are tracked as local markdown files under `.scratch/<feature-slug>/`. `.scratch/` is gitignored local agent state; promote implemented decisions to maintained docs when they describe the current system. See `docs/agents/issue-tracker.md`.

### Triage labels

The default canonical triage labels are used unchanged. See `docs/agents/triage-labels.md`.

### Domain docs

This repo uses a single-context domain layout with root `CONTEXT.md` and ADRs under `docs/adr/`. See `docs/agents/domain.md`.
