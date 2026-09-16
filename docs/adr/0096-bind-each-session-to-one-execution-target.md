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
