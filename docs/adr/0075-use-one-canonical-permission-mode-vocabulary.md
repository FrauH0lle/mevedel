# Use one canonical permission-mode vocabulary

Status: accepted

Permission modes are named `ask`, `edits`, and `full-auto` in the UI, internal
state, configuration, persisted sessions, tests, and documentation. The middle
mode names the capability it automates instead of overloading `auto`, while one
vocabulary removes translation and search ambiguity. Mevedel accepts no aliases
or legacy persisted values. New sessions inherit the global `ask` default
unless the user deliberately selects another mode.
