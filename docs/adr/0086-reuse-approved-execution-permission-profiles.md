# Reuse approved execution permission profiles

Status: accepted

## Current decision

Bash and batch Eval resolve command authority and child-confinement authority
in one interaction. The card presents the operation plus every requested
additive capability. Session and workspace approval initially select the
complete profile; the user may narrow command, network, or path
remembering before settling it.

Reusable approval stores network and filesystem requirements on a
recognized single-segment Bash pattern or exact Eval expression. Compound Bash
commands keep their generalized operation rules, but bind the profile to the
complete command so one segment cannot inherit another segment's capability.
Each direct profile authorizes its own paths for matching executions. A later
matching `use_default` invocation automatically receives the union of its direct
approved profiles without companion resource grants. Independent path authority
remains in the shared resource-grant store. Revoking a profile removes its
command-scoped access; revoking an independent grant removes only that grant.
Explicit additions are merged with remembered additions, while
`require_escalated` remains a separate complete bypass.

Only direct session, workspace-persistent, and global user rules contribute
profiles. Invocation/request delegation cannot broaden confinement. Live Eval
never receives child permissions. The mechanism is command-pattern based and
contains no package-manager or workload-specific policy.

Profiles preserve exact versus recursive filesystem extent. The card's selected
current-call extent is independent of which capabilities the user remembers.
For reusable commands, the initial selection includes the whole requested
profile, including capabilities already available for this call. The path
selector offers command scope, independent path access, or no remembering;
non-reusable commands default to no remembered paths and require explicit
selection of independent access. A selected path is stored in only one scope.
Disabling command remembering clears command-scoped paths and network authority
without converting them to independent grants.

Target replacement strips filesystem entries from session profiles and invalidated
frozen authority alongside session resource grants. It preserves command and
network approvals and does not rewrite durable workspace/global configuration.

Confinement refuses exact directory writes and exact reads beneath inaccessible
masks that Bubblewrap cannot represent without widening authority. Explicit tree
grants can admit protected Git metadata. File masks use private mode-000 files,
and approved exact file mounts replace only their matching masks. These are
current representability limits, not invitations to broaden a request.

Remembering does not infer unknown requirements, turn a failure into a prompt,
or replay a process. A model must still issue a new invocation when it discovers
a capability that no matching approved profile contains. The complete
effective profile is resolved before spawn, and a child that may have started
is never retried automatically.

## Rationale and consequences

Operation-specific reuse removes repeated prompts without making all future
commands inherit one operation's network or path authority. A self-contained
profile also removes the duplicate configuration and hidden dependency that made
a complete-looking command rule insufficient. Native tools never inherit a
command profile. Resource grants and profiles remain independently revocable;
neither acts as a revocation switch for the other. New requirements need a fresh
invocation; a partial failure never causes an automatic broader retry.

The [permissions manual](../permissions.md) owns the card, matching rules, and
scope controls. Regression evidence for these boundaries follows.

## Decision history

- **ADR 0079** required a fresh, justified model invocation before requesting
  missing execution authority. Failed confined results carried a conditional
  retry hint; successful and semantic non-error outcomes did not. Its safety
  constraint survives: a failure never creates a prompt or replays a command.
  When a required backend is unavailable, additive authority cannot repair it;
  only a new explicit full-escalation request can ask to bypass confinement.
- **ADR 0082** allowed explicitly requested additive network access in
  full-auto, while ask and edits required direct user authority. That mode
  distinction remains, but callers no longer need to repeat capabilities
  already supplied by a matching approved profile.
- **ADR 0083** stored reusable network authority in capability-qualified tool
  rules and filesystem authority in a shared grant store. One card resolved the
  operation and unresolved additions, with separate remembering toggles; denial
  rejected the complete invocation. Workspace remembering never created global
  policy. These separation and approval constraints remain. The old rule merely
  matched an explicit capability request; it did not attach requirements to a
  later default invocation.
- **ADR 0086** replaced that repeated-request requirement with automatic reuse
  of directly approved profiles. Initially it required a separate filesystem
  grant alongside each profile, so revoking either half removed access. The
  initial record specified this reduction in caller bookkeeping without citing
  a measurement for the replacement.

### Profile and mount refinements

Profiles later gained optional recursive filesystem requirements because an
exact-only profile could not reuse approved directory-tree authority.  A
recursive requirement then required a sufficient recursive direct grant;
exact and recursive entries remained separate identities.

The permission card gained direct selection of that extent. The selected scope
fed the current invocation independently from the remembering toggles, and
remembered resources retained the same scope in both the profile and direct grant.
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

### Self-contained command profiles

Manual configuration of the Eask cache profile exposed the duplicate authority:
its `:file-system` entry was ineffective without the same path in
`:resource-grants`. The rule looked complete but had a hidden dependency, and
remembering a command also granted path access independently of that command.

Profiles now authorize their own paths. Independent grants are an explicit
alternative in the permission card, rather than an implicit side effect. The
file shape is unchanged, but existing profiles no longer depend on companion
grants; old independent entries are not removed automatically. Regression tests
cover command-only authority, independent selection, revocation, target
replacement, and actual confined cache reuse without companion grants.
