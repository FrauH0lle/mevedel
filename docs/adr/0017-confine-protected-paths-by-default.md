# Confine protected paths by default

In Ask and Edits, child-process confinement reinforces protected-path permission checks so
indirect access cannot bypass shallow Bash operand analysis.  Project `.git`
is read-only by default, preserving Git inspection while requiring additive
write authority for mutation.  Other protected paths, including `~/.ssh` and
`~/.gnupg`, are inaccessible by default because read-only access still exposes
credentials; user-configured protected paths inherit this fail-closed default
unless explicitly marked read-only.  Protected restrictions are applied after
writable-root mounts, and an approved invocation receives only the required
path-specific read or write exception rather than bypassing the sandbox.  The
public configuration is an alist from protected-path glob to `read-only` or
`inaccessible`; a compiler resolves those policies into concrete sandbox paths
rather than treating globs as mount instructions. Literal directory discovery on
local Linux uses GNU `find` when available, with NUL-framed paths, matched-tree
pruning, unreadable-tree protection and no interior symlink following. Managed
Bash and one-shot helpers run these scans as owned asynchronous subprocesses.
Cancellation or owner teardown prevents launch. Before applying the result and
launching, the editor checks unchanged permission rules and grants, confinement
mode, execution target, source-buffer liveness and discovery-root identity.
The protected policy must also be unchanged; otherwise preparation refuses.
Discovery remains fresh per launch, with no cross-launch cache; a native scan
failure or truncated result refuses preparation. Other patterns and remote
targets retain synchronous discovery. Determinable protected roots
that do not yet exist become protected creation targets beneath writable
parents so an opaque child cannot evade policy by creating the path after
launch.  Protected-path approval may grant the exact read or write capability
once, for the session, or persistently; such a resource grant suppresses the
covered path prompt without authorizing any command form.  Grants are stored
separately and never rewrite protected-path policy, so revocation immediately
restores the underlying confinement restriction.

Final path validation remains in the editor and checks the filesystem afresh.
Writable-symlink checks inspect only prefixes inside the shallowest covering
writable root. Restriction sorting computes each depth once. Git pointer files
skip editor coding-detection hooks such as EditorConfig; they still use ordinary
text decoding. These optimizations do not cache path or permission observations
across preparations or remove canonical-target validation.

## Temporary-root boundary

Protected-path glob discovery does not traverse a discovery root equal to the
execution target's temporary directory. A workspace nested there remains a
separate discovery root and is still checked.
A read-only mount whose source has disappeared is skipped. Other confinement
preparation or launch failures follow
[ADR 0116](0116-return-failed-confined-launches-without-retry.md).

Full Access deliberately removes these default protections. Explicit hard
denies remain. Native Edits reads mirror the ordinary readable boundary and
retain inaccessible credential masks; read-only protection does not prohibit
inspection.

## Decision history

The September 2026 permission audit found repeated outside-read approvals and
the user explicitly chose Full Access semantics. Protected defaults therefore
apply to Ask/Edits rather than silently limiting Full Access.


A September 2026 unattended-session profile found protected glob discovery walking
all of `/tmp` before every Bash launch. A transient helper tree containing `.git`
disappeared before launch, causing a Bubblewrap mount failure and, under the
then-current fallback policy, unconfined execution. Skipping temporary-root
search avoids that cost and race: the child already owns that scratch area, so
protecting a repository found there adds little. Missing read-only sources are
skipped. The later no-retry boundary in ADR 0116 independently removed unrestricted
replacement after confined preparation begins.

A September 22 graphical replay associated the first Bash admission with a
777-ms delay. A matched checkout scan found the same 314 protected candidates
using both implementations: median Lisp discovery took 659 ms, native discovery
200 ms. Native traversal initially remained synchronous. A later graphical
capture still measured 192–194 ms in these scans, motivating the owned
asynchronous handoff. A matched full-preparation replay over 323 restrictions
reduced median event-loop delay from 289 to 106 ms; total preparation changed
from 309 to 332 ms. Path resolution and mount planning remain foreground work.
Cancellation uses the existing execution registry and process lifecycle, including
one-shot teardown. Canonical target and final launch checks remain in place.

A subsequent graphical capture measured discovery callbacks up to 226 ms,
including garbage collection. Final restriction processing dominated those
callbacks. Five alternating compiled-function replays over the same 323
restrictions, with the graphical setup's EditorConfig coding hook enabled,
reduced median full-preparation heartbeat delay from 111 to 102 ms and editor
allocation from 5.28 to 4.92 MB. The gain is modest; canonical filesystem checks
remain a foreground cost.

### 2026-09-22: avoid materializing output lines for launch markers

The graphical request profile attributed roughly 127 MB of temporary allocation
to splitting, inspecting and rebuilding tool output around the private launch
marker. Marker detection and removal now scan for exact, case-sensitive lines.
A 6.4-MB, 100,000-line replay reduced this phase from about 40 to 14 ms and from
29.9 to 12.8 MB of allocation. Output separators and result metadata are preserved;
case differences and embedded marker-shaped text remain ordinary output.
