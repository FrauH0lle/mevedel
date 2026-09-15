# Cockpit surfaces follow three archetypes

Status: accepted. Incorporates ADR 0006.

## Current decision

Each cockpit surface uses one of three forms:

- **Action menu:** a transient with a compact identity header, an additional
  state line when needed, and columns of verbs.
- **Table cockpit:** a tabulated-list surface with identity, scope, counts/state,
  and a `? keys` pointer in its header. Growing resource collections belong here.
- **Info panel:** a read-only report with named sections, opened from the
  surface that owns it. General reports use outline headings and foldable
  sections; memory details use a section index and a reading pane.

Plugins, skills, tools, remembered authority, and worktree lists share generic
table plumbing in `mevedel-cockpit.el`: owner context, stable selection, refresh,
row details, and back navigation. Resource modules own rows, actions, state
changes, and detail content. The helper does not interpret their semantics.

Shared table defaults are `g` refresh, `?` help, `q` back, and RET row details;
a surface may add its specific actions. Action menus expose their keys directly,
and use `i` for information where offered. `b` is not a back alias, leaving it
available for actions such as Goal budget editing. Generated key help is the
fallback when a table has no authored help report.

Repeat-heavy view navigation has a sticky submenu. Segment, display, and query
motion reuse view key meanings such as `n`, `p`, and TAB. The
[view manual](../view.md) describes user navigation and surface ownership.

Information panels share `mevedel-report.el`. Owners supply exact content and
explicit section identities, and retain responsibility for validation and actions.
The renderer owns only faces, visual wrapping, folds, reading positions and
windows. It uses built-in Emacs buttons and windows plus existing mevedel
fontification; pacfiles-mode is visual inspiration, with no dependency.

Memory proposals, journal captures and running consolidation expose their own
content groups. The memory index moves above the reader in narrow windows.
Opening and refreshing inspectors preserve the composer and table state.

## Rationale and consequences

Text that can grow should not become a transient header. Resource lists need
selection and row-local actions; detailed policy needs a readable information
surface. Shared table mechanics reduce duplicated UI code while keeping domain
state at its owner. A new table can obtain standard navigation and help without
inventing its own interaction vocabulary.

Help is useful where keys are otherwise hidden; repeating already visible
transient bindings in another buffer adds no information. Stable navigation keys
reduce relearning across surfaces, while resource actions remain explicit.

## Decision history

- **ADR 0006 replaced separate hand-built listings with shared table plumbing.**
  The existing plugin table was the first proof of the shell; skills and tools
  used the same owner/selection/refresh pattern. It deliberately avoided a
  generic framework that understands resource semantics.
- **Remembered permissions moved from transient descriptions and completing-read
  revocation into the table pattern.** The unbounded list and per-rule revoke
  action fit rows; the former UI made users read the same authority strings twice
  to act once.
- **ADR 0105 standardized the surrounding surface shapes.** Headers had grown
  inconsistently: one sentence for Permissions, one line for Model, five for Goal,
  nine for Worktree status, and twelve or more for Preset. A failing preset also
  appeared as one ordinary-looking row among nine. Compact identity headers,
  selectable tables, and separate detailed information replaced that ambiguity.
- **Navigation moved from closing top-level commands into a sticky submenu.**
  Repeated motion had required six menu openings where one now suffices. Matching
  the existing view keys replaced a second vocabulary for the same commands.

- **Information panels gained section structure.** Long memory reports mixed
  decisions, raw identifiers, diffs and evidence into one crowded block; even
  session info lacked clear grouping. Native headings and folds make general
  reports scannable. A separate index makes long memory sections directly
  reachable while preserving exact records. Resource collections retain their
  tabulated presentation and existing actions.
