---
name: "Trace unreadable addresses back to discovery"
description: "Check discovery-to-consumption at root and descendant scopes without weakening valid access refusals"
type: feedback
---

# Trace unreadable addresses back to discovery

**Rule:** an unreadable resource address is not necessarily model-invented. Trace the address to the discovery output before blaming the caller or weakening the consumer's refusal.

**Why:** recorded 2026-10-12, Glob advertised private `.mevedel-pending-executions` addresses that subsequent Read/Grep correctly refused. An isolated fixture passed bare-root Glob and descendant Grep, while descendant Glob returned the private address. Repeating with a loaded-source identity assertion preserved the two-pass/one-failure outcome.

**When to apply:** changes or investigations involving resource listing, search, address generation, and read access.

Test discovery-to-consumption behavior at **both root and descendant scopes**. A bare-root listing test does not cover descendant-path resolution, and a valid refusal should remain a refusal while the inconsistent discovery path is investigated.

**Scope limit:** the reproduction identified the repository source loaded by the isolated fixture. It did not establish the version or behavior of the installed live editor.
