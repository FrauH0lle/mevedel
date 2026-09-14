# Plugin activation enables implemented components

Status: accepted

## Current decision

Plugin activation is workspace-scoped and enables every implemented component
from the selected source. Executable hooks require a concise consent summary of
that executable surface before enabling. Activation binds plugin name plus source
root; a higher-precedence same-named source cannot inherit it silently. Listing
shows the winning source, shadowed duplicates, and any activation conflict, with
explicit confirmation to switch sources.

Updates preserve activation when name and source stay the same. Hook consent is
fingerprinted separately: changed hook files, events, matchers, commands, or
functions require renewed consent while skills remain enabled. Workspace runtime
data is keyed by plugin name, so switching sources reuses that data directory.

The plugin management buffer uses the shared tabulated cockpit, with row actions
and RET details. Slash commands provide local command access; short confirmations
use the echo area. Mutations refresh visible skill/hook state in live consumers.
The [skills and plugins manual](../skills.md#local-slash-commands) owns commands,
installation locations, and the managed-only update/removal boundary. Disabling
is workspace deactivation; uninstall does not remove workspace runtime data.

## Rationale and consequences

One activation command matches the expectation that enabling a plugin makes its
implemented components available. Hook-specific overrides remain available for
advanced use, without making separate skill/hook toggles the normal workflow.
Source binding prevents a same-name replacement from silently gaining authority.
Hook fingerprints preserve consent at the executable boundary without disabling
unchanged skills after every update.

## Decision history

The initial record deferred transient UI while establishing command behavior and
a dedicated multiline plugin buffer. The current surface uses the shared table
shell under [ADR 0105](0105-cockpit-surfaces-follow-three-archetypes.md), retaining
resource-owned rows/actions. The earlier first-implementation checklist is no
longer a plan; its implemented contracts live in the manual.
