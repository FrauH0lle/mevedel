---
name: handoff
description: Compact the current conversation into a concise handoff for another agent or future session. Use when user asks for a handoff, transfer note, continuation summary, or 'what should the next agent know?'
argument-hint: "What will the next session be used for?"
---

# Handoff

Write a handoff so a fresh agent can continue the work without replaying the whole conversation.

Default to producing the handoff in the chat. Do not create a file unless the user explicitly asks for one. If the user asks for a file, use a project-local scratch/handoff path or a user-approved path; do not use `/tmp` or generate paths with shell commands.

Use this structure:

1. **Current goal** - what the next session is trying to accomplish.
2. **Changed files** - exact paths and what changed in each.
3. **Decisions made** - settled constraints, tradeoffs, and rejected alternatives.
4. **Failed approaches** - attempts that did not work and why.
5. **Validation run** - exact commands/checks run and their results.
6. **Remaining risks** - unresolved issues, flaky checks, or unverified assumptions.
7. **Pointers** - session transcripts, plans, issues, PRDs, ADRs, commits, or diffs that already contain detail.
8. **Recommended next skill** - name the next skill, if any, and why.

Do not duplicate content already captured in other artifacts. Reference existing artifacts by path or URL instead.

If the user passed arguments, treat them as the next session's intended focus and tailor the handoff to that focus.
