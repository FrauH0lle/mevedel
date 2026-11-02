# Bind each session to one execution target

Status: accepted

Each session owns one immutable execution target: local, a supported remote host, or a supported container. Composite TRAMP targets are experimental. Target-native paths are qualified against that target before authorization, and paths naming the client machine or another target are denied rather than offered as additional authority. Workspace file effects, project command hooks, project-authored executable resources, child processes, dependency probes, Git worktrees, and Bubblewrap confinement execute on the target; user resources and Emacs Lisp remain local according to their origin. This preserves one coherent filesystem, process, permission, and sandbox authority per session instead of making every tool reason about cross-target effects.

Permission files are target-owned configuration at both scopes. The target
user's `~/.mevedel/permissions.el` and the project's `.mevedel/permissions.el`
both contribute rules and resource grants, with paths interpreted on that
target. Local sessions honor `mevedel-user-dir`; remote sessions never fall
back to the client's file. The permissions cockpit identifies each source and
revokes entries in that source. Approval prompts continue remembering only
session or workspace authority.

Workspace-store exact grants may also use bare or `./` relative paths in
`:resource-grants` and command-profile `:file-system` entries. `.` and `./`
name the workspace root. Loading binds each relative entry to the store-owning
workspace's canonical, target-qualified root, not `.mevedel/` or an agent's cwd.
A private runtime `:workspace-origin` record retains that root and relative
spelling. Only origin-marked entries serialize as `./…`; existing absolute and
home-relative intent is preserved. Global and session exact-path syntax and
tool `:path` glob matching are unchanged.

Relative authority rejects empty paths, every `..` component, foreign target or
resource prefixes, and symlink components below the captured canonical root.
Changing an agent or fork's cwd to another worktree does not re-anchor authority
loaded from the source store. Linked-worktree Git metadata outside that root
requires separate grants. Frozen authority retains the origin; permission
files and durable session authority never serialize the private record. Resume
reloads workspace-derived authority rather than restoring an unbounded session
copy.

Containment is checked before pure permission matching and again at native
consumption against the authorized canonical path. Native operations retain
their existing concurrency boundary, not a new OS-wide atomic guarantee
against replacement between check and use. Confined launch rechecks before
canonical-path and inode capture, then relies on existing descriptor-pinned
validation for later replacement. A filesystem change can make a relative
grant unusable without invalidating its configuration or unrelated denies;
editing and revoking that entry remain possible. Invalid permission files contribute no
authority: one malformed grant rejects the whole file, with a warning naming
scope, rule/tool or resource-grant section, grant index, rejected path, and
reason, once per file version.

## Decision history

Global permission rules originally came from the client-local user directory,
even for remote sessions, and global resource grants were parsed but ignored.
Inspection of the global-grant loader exposed this inconsistency: identical
store formats had different effects, and a remote project's authority changed
with the connecting client's configuration. Both permission stores now belong
to the execution target and contribute both kinds of authority. This matches
the user's expectation that a remote machine supplies its own permissions and
keeps path and configuration ownership in the same domain. Existing client
rules are not copied to remote hosts; previously ignored global grants become
effective on the machine owning their store.

### Bind portable workspace grants to their source

In October 2026, project-local grants needed to survive checkout relocation
without rewriting absolute paths. Resolving relative text against the active
agent cwd would instead let a child working in another worktree redirect the
source store's authority. Expanding it once to an ordinary absolute grant would
also lose its containment restriction during coverage, freezing, or launch.

Workspace-relative entries therefore retain a runtime origin through every
authority consumer. The workspace root is canonicalized, but symlinks below it
are refused rather than followed, so relative intent cannot authorize an
external target. This is an explicit workspace-store extension, not a change to
global/session paths or glob rules. Precise whole-file diagnostics make invalid
entries repairable without partially accepting a malformed authority store.
