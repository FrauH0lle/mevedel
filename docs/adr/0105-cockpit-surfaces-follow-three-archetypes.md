# Cockpit Surfaces Follow Three Archetypes

Every cockpit surface should be exactly one of three things, and its header,
keys, and width follow from that choice:

- **Action menu** — a transient. Its header is one identity line, plus a second
  line only when session state is off-nominal. Its body is columns of verbs.
- **Table cockpit** — a `tabulated-list` surface. Its header line is identity,
  scope, counts and state, then a `? keys` pointer. The key list itself lives in
  `?` help, not in the header.
- **Info panel** — a read-only help buffer of aligned `Label  value` rows,
  opened with `i` from the surface that owns the state.

The rule that assigns an archetype is that text which can grow is not a header.
Remembered authority grows per rule, preset policy grows per tier and workload,
and a Goal record grows with its blocked reason. Surfaces had been rendering
those inside transient descriptions, so a menu header meant five different
things depending on which surface was open: one sentence in Permissions, one
line in Model, five in Goal, nine in Worktree status, twelve or more in Preset.
The cost was not only length. A preset that fails to resolve reported itself as
one row among nine identical-looking rows, in the same face as the rest.

Shared key contract across all surfaces: `q` goes back, `?` shows the keys, `g`
refreshes, `RET` acts on the selection, and `i` opens the surface's info panel.
`b` never means back, which is what lets the Goal surface keep `b` for its
budget without ambiguity.

`?` is added only where the keys are not already visible: table cockpits, the
top-level cockpit (whose help documents slash commands), and worktree status
(whose help documents the list surface's keys). A transient already displays
every key it binds, so a help buffer repeating them would be noise. Surfaces
without their own `:help-function` fall back to key help generated from their
`:keys` spec, so a new surface answers `?` without writing help first.

Navigation moved out of the top level into its own sticky submenu. Segment,
display, and query motion is repeat-heavy, and the top level closes after each
command; a submenu whose entries are all `:transient t` turns six menu openings
into one. Its keys now match the ones the view buffer's own keymap binds
(`n`/`p`/`TAB`) instead of teaching a second vocabulary for the same commands.
