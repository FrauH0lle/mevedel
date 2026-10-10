# Collaboration latency — measurements, 2026-10-10

Question: two people editing a whiteboard on the public host saw about 0.5 s
between one person's input and its appearance on the other's screen. Where
does the time go, and how does it scale with more users?

## Setup

- Guests: browsers on the internet (Wi-Fi, other provider). Desktop → relay
  RTT ≈ 12–25 ms (`curl` connect 25 ms, cursor round trip 12.5 ms).
- Relay: 192.168.24.41 (yunohost, nginx → `127.0.0.1:7466`). Config fine; no
  buffering or extra hops.
- Host: 192.168.24.43, rootless Podman containers (`mevedel-public`,
  `mevedel-private`), 4 cores, Emacs 31.1, `gc-cons-threshold` 64 MB.
  Host ↔ relay: 0.1 ms (same Proxmox).
- Deployed code: `master` (`6c7d6e44`, then `e65137c` after today's update).
  The `artifact-store` branch (`ffea346`) was measured in a separate
  `mevedel-bench` container on the same host with its own state and project.

## Method

`benchmark/collab-latency/` on branch `collab-latency`:

- `loadbot.mjs` — N headless guests speaking the sealed viewer protocol 3
  through the real relay, one process and one clock. Scenarios: `edit`
  (Yjs updates through the real save path), `presence` (cursor), `prompt`
  (chat prompt until its record reaches the others).
- `host-trace.el` — temporary `:around` advice on the collaboration,
  editing, publication and control-program functions; per-stage p50/p95 and
  a span timeline. Loaded into the live daemon over `emacsclient`, removed
  afterwards.
- `local.sh` + `local-host.el` — the same bot against a local relay and an
  isolated batch host of any checkout (master vs branch on one machine).
- `projection-bench.el` — publication projection cost vs transcript length.

"seen" = guest A sends → guest B receives the frame. It excludes the browser:
the editor's 300 ms send interval and rendering add to it.

## Results

### Whiteboard edits (seen by the other guest, p50 / p95)

| Code | Where | 2 guests | 10 guests | 10 guests, 3 writers |
|---|---|---|---|---|
| master | live host .43 | **220 / 249 ms** | — | — |
| artifact-store | bench .43 | **119 / 163 ms** | 115 / 154 ms | 92 / 160 ms |
| master | desktop, local relay | 74 / 111 ms | — | — |
| artifact-store | desktop, local relay | 16 / 56 ms | 18 / 56 ms | 57 / 61 ms |

20 guests / 5 writers (local, branch): ack 68 ms, seen 34 ms p50.

Perceived latency today ≈ browser send wait (0–300 ms, mean 150) + 220 ms +
render ≈ 370–520 ms, matching the ~0.5 s observed.

Host stages per edit on .43:

- **master:** `mevedel-shared-editing-commit-file` **172 ms** of 220. Every
  edit publishes a whole session generation: rebuilds the session sidecar
  and does a durable multi-file publish. All other host work is ≈ 10 ms.
- **branch:** `artifact-lease-ensure` **25.7 ms** (reads the target clock
  through a control program on every edit), `artifact-lease-write` **24.8 ms**
  (verify+write control program), `editing--changed` **33 ms with 10 guests**
  (one separately sealed unicast frame per guest, ~3 ms each, linear in
  guests; the frame is only ~4.7 KB, history is capped at 32).
- Process spawning in the rootless container costs ~4× the desktop: the
  same control program takes 6 ms locally and 25 ms on .43.

### Cursor presence

12.5 ms p50 (2 guests) → 21.5 ms p50 / 50 ms p95 (20 guests, 5 pointers) on
.43. Network-bound. The host's 45 ms per-guest rate limit drops 6–14 % of
samples under load, by design.

### Chat prompts

Prompt → record at other guests: 140–172 ms p50 on a short session. On the
host, 118 ms of that is inside `mevedel-view--drain-follow-up` before the
prompt is submitted: four synchronous control programs (session lease
proof, target clock, …). The publish itself is 2 ms. Request preparation
(74 ms, including `mevedel-skills-scan` twice) runs after the publish and
does not delay visibility.

### Transcript length and streaming

Every publish (each 100 ms while a reply streams, each prompt, each status
change) re-projects the whole transcript.

| Turns (synthetic) | Records | Project, desktop | Publish, .43 |
|---|---|---|---|
| 10 | 51 | 3 ms | — |
| 100 | 501 | 31 ms | — |
| 200 | 1,001 | 62 ms | **360–500 ms** |
| 400 | 2,001 | 133 ms | — |

While a 1,015-record session streams (simulated stream, 10 publishes per
5 s, Emacs ≈ 80 % busy):

- whiteboard edits in the same daemon: p50 221 → **373 ms**, max 708 ms;
- cursor presence: p95 14 → **414 ms**, half the samples dropped (bursts
  after each stall trip the rate limit);
- a guest joining that room: **3.1 s** for the welcome snapshot.

With a short transcript, real streaming did not measurably affect edits
(p50 221 → 228 ms).

### Control programs block the single Emacs thread

Each `mevedel-session-control-fs-run-program` is a synchronous `bash` with a
20 KB script that forks its own subshells: 8–35 ms each on .43. One short
prompt turn ran ~50 of them in 6 s, blocking Emacs for ~0.9 s in total:
publication lease renewal and commits, publication generations and
collection, recovery markers, journal capture, state cleanup. Every frame
for every room in that daemon waits while one runs.

## Recommendations, ranked by perceived gain

1. **Ship artifact-store.** −100 ms per edit on .43 (220 → 119 ms).
2. **Editor send interval** (`shared-editing/editor.mjs:2447`,
   `setInterval(flush, 300)`): send as soon as nothing is in flight and
   coalesce while a save is pending. −150 ms mean, −300 ms worst. Rebuild
   the bundles and redeploy the relay.
3. **Lease clock per edit** (`mevedel-artifact-lease-ensure` →
   `mevedel-session-durability--target-time`): trust the held lease while
   its local deadline, minus a margin, has not passed; ask the target only
   near expiry and on renewal. −25 ms per edit on .43.
4. **One broadcast per change** (`mevedel-collaboration-editing--changed`):
   send the update once with peer 0; guests not viewing the item ignore it.
   −3 ms × (guests − 1); 20 guests ≈ −60 ms.
5. **Incremental publication** (`mevedel-collaboration--publish` →
   `--project-records`): re-project from the first changed segment, not the
   whole buffer. Stopgap: scale the publish delay with the last projection
   cost so streaming cannot saturate Emacs. Removes the long-session stalls
   (+150 ms p50, presence p95 414 ms, 3 s joins).
6. **Control-program cost** (`mevedel-session-control-fs`): a persistent
   helper per target instead of `bash` + 20 KB script per program, and/or
   async execution for background maintenance (lease renewal, collection,
   recovery markers, journal). Cross-cutting; removes most host-side
   stalls, including the 118 ms before a guest prompt appears.
7. **Single workspace editing queue** serializes writers (local: 3 writers
   17 → 57 ms). Not visible on .43 yet; revisit after 3 and 6.

Estimated perceived edit latency (2 users): today ≈ 370–520 ms; after 1 ≈
270 ms; after 1–2 ≈ 135 ms; after 1–4 ≈ 100 ms.

## Side findings

- **Stale busy flag:** `*mevedel:Project AGENTS.md Setup@snt-app*` kept
  `mevedel--current-request` set after its turn finished on 2026-10-09
  15:04 (summary written, no request process). `./update` and the nightly
  update refused to run until `--force`.
- `mevedel-skills-scan` runs twice per request (13 ms each on .43).
- The deployed relay and nginx add nothing measurable.

## Not measured

Browser rendering, artifact comments, file/store requests and lobby
listing latency. They share the same transport and host thread, so items 5
and 6 apply to them as well.

## Reproduce

```sh
npm ci --prefix shared-editing
# against a live room (full link):
node benchmark/collab-latency/loadbot.mjs LINK edit --guests 10 --writers 3
# master vs branch locally (byte-compiled checkouts):
benchmark/collab-latency/local.sh PATH/TO/CHECKOUT edit --guests 2
```

## State left behind

- Desktop: SSH key `~/.ssh/id_ed25519_mevedel_lan`, `mevedel-relay` and
  `mevedel-host` aliases (root) in `~/.ssh/config` (backup
  `config.bak-20261010`); worktree `.scratch/worktrees/master-bench`.
- GitHub: pushed `master` `e65137c`.
- .41: key in root's `authorized_keys`; nothing else changed.
- .43: key in root's `authorized_keys`; hosts updated to `e65137c` with
  `--force`; project `public/latency-lab` with lab sessions and test
  whiteboards (`lab-2` carries a synthetic padded transcript); `/tmp/*.el`
  helper files inside `mevedel-public`; stopped container `mevedel-bench`,
  its volume `mevedel-bench-state` and `/home/mevedel/mevedel-host/bench/`.
