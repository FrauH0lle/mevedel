# Confine protected paths by default

Child-process confinement reinforces protected-path permission checks so
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
rather than treating globs as mount instructions.  Determinable protected roots
that do not yet exist become protected creation targets beneath writable
parents so an opaque child cannot evade policy by creating the path after
launch.  Protected-path approval may grant the exact read or write capability
once, for the session, or persistently; such a resource grant suppresses the
covered path prompt without authorizing any command form.  Grants are stored
separately and never rewrite protected-path policy, so revocation immediately
restores the underlying confinement restriction.

## Temporary-root boundary

Protected-path glob discovery does not traverse a discovery root equal to the
execution target's temporary directory. A workspace nested there remains a
separate discovery root and is still checked.
A read-only mount whose source has disappeared is skipped. Other confinement
preparation or launch failures follow
[ADR 0116](0116-return-failed-confined-launches-without-retry.md).

## Decision history

A September 2026 unattended-session profile found protected glob discovery walking
all of `/tmp` before every Bash launch. A transient helper tree containing `.git`
disappeared before launch, causing a Bubblewrap mount failure and, under the
then-current fallback policy, unconfined execution. Skipping temporary-root
search avoids that cost and race: the child already owns that scratch area, so
protecting a repository found there adds little. Missing read-only sources are
skipped. The later no-retry boundary in ADR 0116 independently removed unrestricted
replacement after confined preparation begins.
