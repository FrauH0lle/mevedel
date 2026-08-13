# Bind each session to one execution target

Status: accepted

Each session owns one immutable execution target: local, a supported remote host, a supported container, or one supported composite TRAMP target. Target-native paths are qualified against that target before authorization, and paths naming the client machine or another target are denied rather than offered as additional authority. Workspace file effects, project command hooks, project-authored executable resources, child processes, dependency probes, Git worktrees, and Bubblewrap confinement execute on the target; user resources and Emacs Lisp remain local according to their origin. This preserves one coherent filesystem, process, permission, and sandbox authority per session instead of making every tool reason about cross-target effects.
