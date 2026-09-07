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

The permission card now selects that extent directly. The selected scope feeds
the current invocation independently from the remembering toggles, and selected
remembered resources retain the same scope in both the profile and direct grant.
A real confined cache fixture verifies the initial approval and three later
default invocations creating new descendants while a sibling stays unwritable.

The initial selection includes capabilities already granted for this call as
well as missing ones. A prompt-to-launch regression found that selecting only
missing additions lost an earlier native read grant and full-auto network
authority from later default calls. Bash and batch Eval now remember the whole
requested profile by default, and offer toggles for every requested capability.
Real confined tests verify both reuse and deliberate narrowing: the current
call retains all its approved resources, while a later call loses omitted
capabilities and is not replayed after a partial failure.

A protected Git fixture exposed a mismatch in the previous mount compiler:
an exact `.git` directory write grant created a writable bind mount for the
whole tree. Bubblewrap cannot express that inode-only authority. Exact directory
write additions and exact reads beneath inaccessible masks now refuse before
launch; already-readable exact directories need no additional mount. Explicit
tree grants permit confined staging and committing. Redundant exact mounts
covered by an approved tree are omitted, while persisted grant identities remain
separate. Failure does not choose a broader extent or retry the operation.

A real linked-worktree regression then found that a granted shared metadata
tree was mounted before its protected worktree-specific child, but its own
read-only remount undid the grant. The mount compiler now omits that exact
read-only restriction when the path has an explicit write grant, preserving
other protected descendants. Staging and committing pass under confinement
with the explicitly selected metadata trees; a separate repository stays
unwritable. Protected Git pointers also resolve the `commondir` file described
by [Git's repository layout](https://git-scm.com/docs/gitrepository-layout).
Failed results disclose the shared metadata location, because the reproduced
Git error omitted it. Discovery supplies neither authority nor a diagnosis of
an arbitrary child failure.

That narrowed-profile test also exposed a protected-file mask that tried to
chmod an already read-only `/dev/null` bind. Bubblewrap refused setup before
the command started. File masks now use private empty files with mode 000 set
before read-only binding; dedicated input descriptors preserve the child's
stdin and never change a host inode. Exact approved file mounts still replace
only their matching masks.

Remembering does not infer unknown requirements, turn a failure into a prompt,
or replay a process. A model must still issue a new invocation when it discovers
a capability that no matching approved profile contains. The complete
effective profile is resolved before spawn, and a child that may have started
is never retried automatically.
