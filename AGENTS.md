# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for
interacting with LLMs during programming. It enables overlay-based
instruction management for AI-assisted development with direct gptel
integration.

## Coding standards

Before designing or implementing a change, editing maintained documentation,
or reviewing a diff, read [CODING_STANDARDS.md](CODING_STANDARDS.md). It owns
the deep-harness, compatibility and migration, ADR, and documentation review
rules, and points to the development guide's conventions.

## Documentation map

Before planning or changing an unfamiliar area, consult the
[documentation map](docs/index.md#documentation-map) and read its relevant
contracts. This file keeps the entry rules and retrieval triggers; the
[working-material policy](#working-material) below sets destinations for plans,
investigations, and reports.

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

Before changing prompts, requests, callbacks, tool calls, presets, buffers,
transcripts, session flow, agents, or coordination, follow the development
guide's [gptel source rule](docs/development.md#gptel-source-rule).

For interactive commands, see [README.md](README.md#usage).

## Working material

- Put ordinary investigations, reports, working notes, drafts, and handoffs in
  `work://shared/`, not `.scratch/`. This applies to full reports as well as short
  summaries and takes precedence over general scratch-storage guidance in the
  backlog and agent docs.
- Keep managed session plans in `work://plans/`; curate selected durable lessons
  into memory. Read-only requests and Plan-mode write restrictions still apply.
- Preserve explicitly invoked skills' configured workflows and destinations,
  including the local issue tracker below. Do not rewrite plugin skills or
  relocate their files merely to follow the ordinary-notes default.
- `.scratch/` is local-only, for skill workflow files and disposable execution
  material such as checkouts and logs. Never track or commit anything under it.
- `.mevedel/shared/`, addressed through `work://shared/`, is untracked,
  machine-local and temporary. Memory consolidation extracts lasting lessons
  from quiet files and then removes them, so never cite shared files from
  tracked documentation; put durable facts in memory or maintained docs.

## Agent skills

### Issue tracker

The Matt Pocock skills' local issue-tracker workflow keeps issues and PRDs under
`.scratch/<feature-slug>/`. Preserve that workflow; its files remain local-only
and must not be committed. This is not the default destination for ordinary
notes or handoffs. Promote implemented decisions to maintained docs when they
describe the current system. See `docs/agents/issue-tracker.md`.

### Triage labels

The default canonical triage labels are used unchanged. See `docs/agents/triage-labels.md`.

### Domain docs

This repo uses a single-context domain layout with root `CONTEXT.md` and ADRs under `docs/adr/`. See `docs/agents/domain.md`.
