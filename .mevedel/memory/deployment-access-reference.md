---
name: "Deployment access and diagnostic references"
description: "Recorded SSH aliases, rootless container layout, and collaboration latency tooling; recheck before use"
type: reference
---

Deployment references recorded on 2026-10-10:

- Desktop SSH aliases: `mevedel-host` for the Emacs containers and
  `mevedel-relay` for the relay server. SSH access was through root; containers
  ran as the `mevedel` user with rootless Podman.
- Host checkout/control directory: `/home/mevedel/mevedel-host/`, with
  `update`, `project`, and `lobby-link` helpers.
- Container names: `mevedel-public` and `mevedel-private`; daemon
  `emacsclient` socket name: `mevedel`.
- Repository diagnostic tooling: `benchmark/collab-latency/`.

The relay was recorded on 2026-08-18 as deployed on Roland's YunoHost server
and used from iPhones. Browser/phone collaboration is actual usage, not just
a demonstration scenario. Current build and viewer contracts belong in
`relay/AGENTS.md` and `relay/README.md`, not this deployment snapshot.

**How to apply:** Use these pointers to locate the deployment and its
diagnostics, then verify current aliases, service/container layout, and
installed versions. These are dated references, not proof of present access
or authorization to deploy, restart services, or mutate remote state. No
host connection was made during import.

**Source:** Claude project memories `mevedel-host-access.md` and
`collab-relay-deployment.md` under
`/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`;
curated for import at the user's request on 2026-10-19. Historical versions,
backup inventories, deployment commands, and Claude permission rules were omitted.
