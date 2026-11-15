---
name: "Deployment access and diagnostic references"
description: "Use the recorded SSH configuration and deployment pointers to locate access details, then recheck current configuration and authority."
type: reference
---

# Deployment access and diagnostic references

## SSH configuration

The authoritative machine-local connection details are in `~/.ssh/config`.

Deployment references recorded on 2026-10-10 named:

- `mevedel-host` for the Emacs containers.
- `mevedel-relay` for the relay server.

A real SSH/TRAMP test reported in the 2026-11-12 journal found that direct batch SSH attempts as `mevedel`, `roland`, and `root` to `192.168.24.43` failed authentication, while **`mevedel-host` succeeded**. At that observation, the alias specified:

- HostName `192.168.24.43`
- User `root`
- IdentityFile `~/.ssh/id_ed25519_mevedel_lan`
- IdentitiesOnly `yes`

Use the configured alias rather than assuming a bare user@IP is equivalent. Inspect the current SSH configuration before use; this dated observation does not establish present reachability or credentials. The user's permission to create a test directory under `/home/mevedel/` applied to that test, not arbitrary future mutations.

## Deployment and diagnostic pointers

Recorded on 2026-10-10:

- SSH access was through root; containers ran as the `mevedel` user with rootless Podman.
- Host checkout/control directory: `/home/mevedel/mevedel-host/`, with `update`, `project`, and `lobby-link` helpers.
- Container names: `mevedel-public` and `mevedel-private`; daemon `emacsclient` socket name: `mevedel`.
- Repository diagnostic tooling: `benchmark/collab-latency/`.

The relay was recorded on 2026-08-18 as deployed on Roland's YunoHost server and used from iPhones. Browser/phone collaboration is actual usage, not just a demonstration scenario. Current build and viewer contracts belong in `relay/AGENTS.md` and `relay/README.md`, not this deployment snapshot.

**How to apply:** use these pointers to locate the deployment and its diagnostics, then verify current aliases, service/container layout, and installed versions. These are dated references, not authorization to deploy, restart services, or mutate remote state.

**Earlier source:** Claude project memories `mevedel-host-access.md` and `collab-relay-deployment.md` under `/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`, curated for import at the user's request on 2026-10-19. No host connection was made during that import. Historical versions, backup inventories, deployment commands, and Claude permission rules were omitted.
