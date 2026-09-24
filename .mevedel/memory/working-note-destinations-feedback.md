---
name: "Put shareable working notes in work:// and memories, not .scratch"
description: "User wants authored notes in work:// and memories; .scratch is PC-local and gitignored"
type: feedback
---

# Put shareable working notes in work:// and memories, not .scratch

User statement (captured 2026-09-21 final message, repeated in each 2026-09-22 session): the model tends to store/share information in ".sratch"; that location is local to this PC and ignored by Git. The intended destinations are `work://` and memories. Each session was a thorough read-only investigation requested with "No code changes yet."

**Why this matters.** Storage capability was already present; `Read("work://")` listed shared notes in every session that tried it. The recurring problem is where authored notes end up, and what that destination actually guarantees.

- Observed result (source inspection): `work://shared/...` resolves under the workspace's `.mevedel/shared/`, survives session deletion, and is shared by sessions retaining that workspace, including mevedel-created worktree sessions. Other `work://` descendants are session-owned. Shared notes are not injected into context automatically; they require discovery/read or an explicit handoff address.
- Observed result: `.mevedel/shared/` is gitignored just as `.scratch/` is, while curated `.mevedel/memory/` is explicitly exempted and tracked. Model inference: switching to `work://shared/` fixes ownership, discovery, and retention integration, but is not by itself synchronization between independent PCs or Git clones.
- Model inference, repeatedly reached, never experimentally isolated: the strongest explanation is conflicting routing guidance rather than a missing capability. Repository guidance sends plans, investigations, reviews, coordination, and PRDs to `.scratch/`; harness guidance sends factual working notes to `work://shared/` and lasting lessons to memory. How much each source contributes to the observed usage frequency was not measured.
- Observed result: `.scratch/directive-workflow-redesign/` already holds 12 tracked PRD/issue files despite the directory's ignore rule, and existing shared notes/handoffs still point into `.scratch` for full evidence. The blanket description "untracked scratch" is inaccurate, and moving only a summary leaves the handoff dependent on material outside the intended storage.

**How to apply.**
- Default authored, shareable working notes and handoffs to `work://shared/`; promote durable lessons to memory. Do not use `.scratch` as the storage for information meant to be shared.
- Keep genuinely local, disposable material in `.scratch`: upstream dependency checkouts, raw test output, benchmark material, generated artifacts. Do not propose a blanket ban or bulk migration.
- Changing a note's destination does not make its handoff self-contained; move or duplicate the linked evidence too.
- Prefer aligning the existing repository and skill routing over adding another reminder, and settle separately whether "shared" must extend across independent PCs or only across sessions/worktrees of one workspace.
- Status: recommendation only. No guidance file, skill, resolver, or note was changed; which material should travel between machines, and by what transport or tracking policy, remained undecided.
