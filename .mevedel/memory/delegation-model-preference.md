---
name: Delegation model preference
description: Prefer GPT 6.1 Sol for cost efficiency unless the selected mevedel preset specifies otherwise
type: user
---

For multi-agent work (pair partners, reviewers, implementers and other
sub-agents), including complex tasks, the user prefers `Codex:gpt-6.1-sol`
with high effort for cost efficiency. The selected mevedel preset takes
precedence over this fallback preference.

**Why:** on 2026-10-20, the user replaced the earlier Astra preference with
GPT 6.1 Sol because it is more cost efficient, while explicitly giving the
selected mevedel preset precedence.
**How to apply:** respect the selected preset's model and effort policy;
do not override it with this memory. When the preset does not specify a
choice, use the Sol/high fallback. The user's explicit selection in a
request (for example a `$pair` partner selection) wins. Model names drift,
so check that they still exist before relying on them.
