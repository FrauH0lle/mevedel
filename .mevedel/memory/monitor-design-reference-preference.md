---
name: "Background-wake design priorities and reference"
description: "Consult Claude Code for background-wake design; the user weighs lost messages more heavily than wake-loop risk"
type: user
---

# Background-wake design priorities and reference

## Design reference

User statement, 2026-10-15, about Monitor / background-wake design in mevedel:

> “ask Opus in Claude Code (run it via the CLI) about the Monitor feature. I think we should follow that implementation when in doubt.”

When resolving uncertainty in this area, consult Claude Code through its CLI and use its Monitor implementation as the default design reference. This is scoped to Monitor and background-wake behavior, not a general instruction to copy every Claude Code policy.

The journal reports that Claude Code 2.1.300's Bash and Monitor descriptions conflicted over single-condition watchers. Preserve such conflicts explicitly instead of presenting the reference as unambiguous. Distinguish retrieved contract text from a proposed mevedel design or an inferred default.

## Message-delivery risk priority

User statement recorded 2026-10-19: lost or ignored agent messages are a bigger risk to the user than agent wake loops. During that investigation, the user asked for comparison with `~/codex`, `~/ccs`, and the Claude Code CLI before deciding.

Apply this priority when evaluating agent-message delivery and idle-agent wake behavior: do not assume that suppressing wakes is preferable if it risks silently leaving messages unread. This is a relative risk preference, not permission for unlimited loops or a standing specification of exact wake semantics. Recheck reference-system behavior for the design under consideration rather than treating the dated investigation as a current guarantee.
