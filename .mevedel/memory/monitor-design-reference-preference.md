---
name: "Use Claude Code as the Monitor design reference"
description: "For Monitor and background-wake design, consult Claude Code through its CLI and use it as the default reference when uncertain"
type: user
---

# Use Claude Code as the Monitor design reference

User statement, 2026-10-15, about Monitor / background-wake design in mevedel:

> “ask Opus in Claude Code (run it via the CLI) about the Monitor feature. I think we should follow that implementation when in doubt.”

When resolving uncertainty in this area, consult Claude Code through its CLI and use its Monitor implementation as the default design reference. This is scoped to Monitor and background-wake behavior, not a general instruction to copy every Claude Code policy.

The journal reports that Claude Code 2.1.300's Bash and Monitor descriptions conflicted over single-condition watchers. Preserve such conflicts explicitly instead of presenting the reference as unambiguous. Distinguish retrieved contract text from a proposed mevedel design or an inferred default.
