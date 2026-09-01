# Reuse approved execution permission profiles

Status: accepted

Supersedes ADR 0079, ADR 0082, and ADR 0083.

Bash and batch Eval resolve command authority and child-confinement authority
in one interaction. The card presents the operation plus every requested
additive capability. Session and workspace approval initially select the
complete profile; the user may narrow command, network, or path
remembering before settling it.

Reusable approval stores network and filesystem requirements on a
recognized single-segment Bash pattern or exact Eval expression. Compound Bash
commands keep their generalized operation rules, but bind the profile to the
complete command so one segment cannot inherit another segment's capability.
Path authority remains in the shared resource-grant store. A later
matching `use_default`
invocation automatically receives the union of its direct approved profiles;
filesystem access is attached only while a sufficient direct resource grant
still exists. Revoking either half removes that access. Explicit additions are
merged with remembered additions, while `require_escalated` remains a separate
complete bypass.

Only direct session, workspace-persistent, and global user rules contribute
profiles. Invocation/request delegation cannot broaden confinement. Live Eval
never receives child permissions. The mechanism is command-pattern based and
contains no package-manager or workload-specific policy.

Profiles later gained optional recursive filesystem requirements because an
exact-only profile could not reuse approved directory-tree authority.  A
recursive requirement is reattached only while a sufficient recursive direct
grant still exists; exact and recursive entries remain separate identities.

Remembering does not infer unknown requirements, turn a failure into a prompt,
or replay a process. A model must still issue a new invocation when it discovers
a capability that no matching approved profile contains. The complete
effective profile is resolved before spawn, and a child that may have started
is never retried automatically.
