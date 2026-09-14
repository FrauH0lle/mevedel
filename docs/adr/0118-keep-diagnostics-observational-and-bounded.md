# Keep diagnostics observational and bounded

Status: accepted

## Current decision

Ordinary telemetry is an append-only stream of bounded lifecycle metadata.
It is diagnostic evidence, never resume state or completion authority. Emission
filters property keys recursively and bounds values; callers classify or hash
payload-derived data before emission. Sessionless work writes workspace
diagnostics without manufacturing a conversation. Persistence failures cannot
fail the user workflow.

Normal runs omit routine per-step and per-handler spans; an explicit profiler
run enables detailed events for its owning session. No-op input validation is
omitted in every tier. Full session debugging separately opts into raw gptel and
view logs, with selected credential headers redacted and owner-only file modes.
Those logs retain other sensitive contents and are not covered by a claim of
complete payload redaction.

Native profiler artifacts describe the client Emacs. Local sessions store them
under their diagnostics directory; remote sessions store them in a client-local
temporary directory and record its absolute path. They are not portable session
state. See the [telemetry manual](../telemetry.md) for commands, fields, artifact
formats, failure handling and the current locality-flag limitation.

## Rationale and alternatives

A field allowlist rejects new, unclassified keys by default; a denylist would
need to anticipate every way a caller might expose payloads. It does not replace
semantic care by the emitter: an allowed string is still just a string.

Append-only readable plists reuse the existing log convention, need no
serializer dependency, and remain incrementally readable after interruption.
Outcome-level normal telemetry limits observation cost while explicit detailed
runs preserve diagnostic depth. Treating every diagnostic as durable session
state would increase storage and transfer costs without improving recovery.

## Consequences

Telemetry can correlate lifetimes and usage but cannot reconstruct authoritative
session state or establish unreported provider usage. Detailed debugging has
extra overhead and a different privacy boundary from ordinary telemetry.
Remote-session profiler output must be collected from the client that produced
it. Profiler start rolls back partial setup; stop halts sampling before fallible
environment capture and reports success only after nonempty artifacts exist.

## Decision history

- A debug capture spent roughly one third of its CPU in `json-pretty-print`
  while streamed response bodies grew into multi-megabyte logs. Raw debug
  entries replaced repeated pretty-printing so capture would not freeze the
  session it was observing. Reproduction tooling also consumed raw entries.
- Saving an 8 MB profile through a remote SSH shell requires transferring
  those bytes through the connection although resume never consults them.
  Client-local profiler artifacts replaced target-side storage for remote
  sessions. This deliberately trades portability for lower diagnostic cost.
- A fallible closing environment snapshot could leave the native profiler
  running after its stop handle was released. Stop now halts sampling before
  that snapshot; start also rolls back setup failures after sampling begins.
  The same ordering keeps snapshot Git and hashing work out of the measured
  profile.
