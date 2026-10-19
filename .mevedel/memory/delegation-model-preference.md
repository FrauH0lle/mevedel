---
name: Delegation model preference
description: Which Codex models to use for partners, reviewers and implementers to save usage limits
type: user
---

For multi-agent work (pair partners, sub-agents), the user prefers
`Codex:gpt-6-astra` with high effort as the partner and for review and complex
tasks, and `Codex:gpt-6.1-sol` with high effort for implementation and
delegation.

**Why:** stated by the user on 2026-10-19, and earlier the same day after
usage limits ran out quickly, to save usage limits.
**How to apply:** when spawning agents without an explicit model choice, pick
these models by role. The user's explicit selection in a request (for
example a `$pair` partner selection) wins. Model names drift, so check that
they still exist before relying on them.
