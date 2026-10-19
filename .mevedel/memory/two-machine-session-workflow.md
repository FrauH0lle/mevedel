---
name: "Desktop-host and laptop-client session workflow"
description: "TRAMP session takeover is real usage; verify both clients and install fixes on the affected side"
type: project
---

Roland uses a desktop to host a project checkout and a laptop to join the
same mevedel sessions over TRAMP. Leaving the desktop working unattended and
taking over from the laptop is a real workflow, not a hypothetical remote
use case. Both machines have their own mevedel installation.

**Why:** Session ownership, leases, publications, control transfer, and remote
round-trip cost affect daily use. A fix loaded on the desktop does not by
itself update the joining laptop.

**How to apply:** Include the two-client case when changing these boundaries
or the session picker. Establish which side executes the affected behavior
and which installation needs updating; do not infer remote correctness from
a local-only check. Re-establish current host addresses rather than retaining
the old literal TRAMP address.

**Source:** Claude project memory `two-machine-session-workflow.md`, recorded
2026-08-17 under `/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`;
curated for import at the user's request on 2026-10-19.
