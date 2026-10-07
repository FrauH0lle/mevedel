# Claude engine: third review and fixes (2026-10-07)

Whole-branch review of `feature/claude-code-engine` (`master..2b4e30f`, 8 commits)
on branch `review/claude-code-engine-c`. Ten independent reviewers fanned out over
ACP transport, MCP bridge, Claude adapter, shared core, persistence/recovery,
lazy loading, collaboration/usage, performance, simplification and tests/docs.
Fixes were made in disjoint file batches, then two adversarial re-reviews
challenged the fix commits. The two earlier reviews covered the engine itself;
most new defects were in the later commits `12fada2` (headless recovery) and
`2b4e30f` (lazy loading), which neither earlier review saw.

Fix commits: `be48e65..901a0c4` (11 commits). No features were removed.

## Fixed: P1 (users would hit these)

- **Provider errors wedged sessions** (`12fada2`). Any error text matching a broad
  regex ("author", "upgrade", "Unauthorized") left a persisted blocking issue that
  nothing in Emacs cleared; every send failed with the old error. Request failures
  are now informational and cleared by the next root request (the retry). Only
  owners that re-check an issue block: Codex login, preset restore, saved-model
  restore.
- **Every abort or failure paused queued input until an explicit resume**
  (regression from master). The pause again applies only to input the failed turn
  affected.
- **A refused guest message locked the host out** and reversed review b's fix.
  Refused guest input is dropped with notices again.
- **Every send reverted model/effort changes** made through the cockpit or presets
  (`mevedel-model-apply-session-policy` ran per send). Back to session start/restore.
- **Claude readiness refused most sends and started Claude twice.** A 30 s TTL probe
  refused any send after 30 s, ran a full throwaway native session, and did not
  resubmit. Readiness is now learned from turn startup and never refuses a send; a
  failed startup is informational (blocking would wedge Goal/plan/directive sends,
  which never pass the composer check, and a missing native history looped).
- **Owner collaboration join crashed for gptel-only users** (`void-variable
  mevedel-claude-code-directory`): no recovery/agents/tasks frames, false failure
  notices to guests, Codex device code never shown. Installation settings now load
  with the always-loaded backend module.
- **Lazy loading broke cold entry points** (`2b4e30f`): first composer send failed in
  compiled installs, resuming any saved session failed, discuss/implement/preview
  directive failed. 77 latent declare-only call edges reduced to 1 (deliberate).
  The cold loading test now covers send, resume, directives and M-x reachability,
  compiled and from source.
- **Headless drain refused every guest message** with `void-function
  mevedel-skills-request-model-policy` when skills-invoke was not loaded yet.

## Fixed: P2

- Tool calls could still overtake context receipts when the MCP socket was read
  before the ACP pipe (fd order is round-robin); already-readable ACP output is now
  pulled first. ADR 0123 states the exact guarantee.
- Queue changes did 1-3 synchronous full session saves each (48-150 ms UI freeze per
  save, also for gptel users); now debounced sidecar-only with one synchronous
  sidecar intent write before dispatch. Runtime/auth events did k^2 saves.
- Agent stderr was discarded on crashes; now shown to the host as a warning (kept
  out of the classified, guest-visible outcome message).
- `lock-file` was a no-op under `create-lockfiles nil` (the user's config):
  installation and Codex auth locks now hold.
- Subagents silently fell back to the default model when their model was not in the
  backend's `:models` list; gptel children no longer copy the whole parent
  transcript into their context.
- Goal no longer retried transient 503 "model unavailable" errors.
- The session converter silently lost Goals from v0.5.9 sidecars; it now adds the
  field for any version and validates the Goal.
- Save As copied the durable queue into both sessions; it now refuses pending input.
- Four integration tests (restart, remote, buddy, memory review) used launch plists
  that drifted from production; they now use the production launch.
- `mevedel-uninstall` left the JSON repair advice (including on `json-parse-string`).
- Collaboration: observer failures re-broadcast on every tool call; preset recovery
  leaked buffer-local permission modes; owners saw issues twice; the login provider
  stuck after sign-in.

## Simplification

- `mevedel.el` hand-written autoloads 229+60 -> 87+51 forms, restoring entry points
  that had dropped out of M-x (buddy modes, pending-input editing, memory list,
  usage).
- Removed: one-method workload generic, ACP request-sender wrapper, duplicate token
  merge, duplicated model validation, intra-branch `node_modules` adapter fallback,
  copied status runner, re-reading of every instruction file per prompt, the
  "input" issue and `:blocked` field, `mevedel-recovery-enqueue`.
- Tests: one Claude session macro, one await helper, one peer path (-435 lines).
  ~70 redundant `boundp` guards removed now that structs owns the shared buffer
  variables. Docs: one owner per contract (-59 lines), ADR 0123 has one decision
  history.

## Performance

- Remote (TRAMP) busy check per ACP event: 24 us / 120 conses -> 0.7 us / 11 conses.
- Codex wait advice no longer rereads the token file on every request.
- Effort `set_config_option` round trip skipped when already current.
- Load: `(require 'mevedel)` 173-191 ms -> 13 ms from `2b4e30f` holds; about half
  of that saving moves to first interaction (net ~30-50 ms through the first send).
- Measured and kept: `emitRawSDKMessages t` costs ~27 us per delta and cannot be
  narrowed without losing live usage; `mevedel-engine-info` dispatch is per turn/tool,
  never per chunk.

## Verification

- Full isolated suite: see "Final run" below. Compile: 241 files, 0 warnings (in a
  disposable copy). `git diff --check` clean. Viewer protocol test passes.
- Every changed test file passes when run alone (several failed alone before).
- Not run: provisioned SSH/Podman remote acceptance (`test/run-remote-acceptance.sh`),
  live Claude/Codex calls, relay redeploy.

## Decisions for the user

1. **Daily `claude install stable`** (`mevedel-claude-code-auto-update`, default on)
   runs against the user's own native CLI install and can move a `latest`-channel
   user back to stable. Kept as a feature; options: follow the configured channel, or
   default auto-update off.
2. **Sidecars written by branch builds `12fada2..04bc103`** that retain `:blocked`
   inputs no longer open, and a persisted blocking "input"/"request" issue from those
   builds is never cleared. The local store has none (51 sessions, all v0.5.4-v0.5.6).
   Option: stop persisting `:recovery-issues` (re-derived at restore/next send).
3. **Working material**: `.mevedel/shared/lazy-loading-scan-2026-10-07.json` (997
   lines, unreferenced), `loading-2026-10-07/results.json` (16.9k lines of raw
   samples) and the superseded 2026-10-06 review handoff could be deleted.
4. **Per-turn process cost** (architecture): every Claude turn spawns the adapter, the
   CLI and `claude auth status`; every tool call spawns a `PreToolUse` hook process
   (~15-19 ms). Narrowing needs a live check.
5. **Claude telemetry gap**: ACP turns emit no provider-call telemetry (now
   documented in `docs/telemetry.md`).
6. Viewer changes need `go build` in `relay/` and a redeploy.

## Final run

At `901a0c4`: full isolated suite **9,676 tests, 0 unexpected, 23 conditional
skips**, 154.5 s, all eight workers exit 0
(`.scratch/test-suite-performance/20261007-141527`). Baseline before the fixes was
9,637 tests, 0 unexpected, but several branch files then passed only through test
order. Byte compilation: 241 files, 0 warnings. No paid model calls were made.
