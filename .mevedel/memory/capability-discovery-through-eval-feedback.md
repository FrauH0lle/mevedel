---
name: "Tool search is not the full capability boundary"
description: "A missing dedicated tool does not establish that a capability is unavailable; consider authorized Emacs Eval."
type: feedback
---

# Tool search is not the full capability boundary

**User correction, 2026-10-31:** absence of a dedicated subscription-status tool does not mean subscription status is inaccessible. Mevedel functions can also be invoked through Emacs Eval.

The subsequent invocation of `mevedel-subscription-usage-provider-fetch` with the Claude Code backend returned subscription limits asynchronously. This was an observed result for that backend on that date, not a guarantee about every provider or current installation.

**How to apply:** when tool search finds no matching capability, consider whether an existing application function exposes it through an available, authorized Eval interface before declaring it unavailable. Inspect the relevant contract and account for asynchronous completion rather than equating invocation with a returned result.

**Scope:** an alternative access path does not grant permission to use it. Assess the expression's effects, keep live probes bounded, and preserve applicable access and mutation restrictions. This correction does not authorize arbitrary Eval or require searching indefinitely for an unavailable capability.
