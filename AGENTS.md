# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for
interacting with LLMs during programming. It enables overlay-based
instruction management for AI-assisted development with direct gptel
integration.

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
- Supersede instead of amending when the decision itself is reversed: keep the
  old record, mark it superseded, and name the ADR replacing it.
- State what moved the decision — a measurement, a failure, a constraint that
  turned out not to hold. "We changed our minds" is not a reason; "the profile
  put this at 21% and the check was redundant with the target-side proof" is.
- Do not defer to an ADR you disagree with. Argue with it, in it.

## Documentation map

Before planning or changing an unfamiliar area, consult the
[documentation map](docs/index.md#documentation-map) and read its relevant
contracts. The `docs/` tree is the maintained working documentation; this file
keeps the entry rules and retrieval triggers.

Each `.el` file also describes its purpose in its `;;; Commentary:` block.

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
  transcripts, session flow, agents, or coordination, consult gptel and
  gptel-agent source and reuse their APIs or patterns. Follow the development
  guide's upstream-source procedure.

For interactive commands, see [README.md](README.md#usage).

## Agent skills

### Issue tracker

Issues and PRDs are tracked as local markdown files under `.scratch/<feature-slug>/`. `.scratch/` is gitignored local agent state; promote durable PRD decisions to maintained docs. See `docs/agents/issue-tracker.md`.

### Triage labels

The default canonical triage labels are used unchanged. See `docs/agents/triage-labels.md`.

### Domain docs

This repo uses a single-context domain layout with root `CONTEXT.md` and ADRs under `docs/adr/`. See `docs/agents/domain.md`.
