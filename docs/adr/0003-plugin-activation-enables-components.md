# Plugin Activation Enables Implemented Components

Plugin activation is workspace-scoped and means enabling every implemented component the plugin contributes in that workspace. We choose a single activation command over separate skill and hook toggles because users expect `/plugin enable NAME` to make the plugin work; executable hooks remain explicit at the risk boundary through a concise consent summary before they are enabled. Hook-specific slash commands remain advanced/scriptable overrides for keeping skills enabled while toggling executable hooks, but they are not the normal activation path. The consent summary should show the risky/executable surface, not dump every manifest field.

Plugin list output should use a dedicated `*mevedel plugins*` management buffer rather than `message` or the chat transcript, because it is multiline diagnostic and management output. The buffer may offer keybindings for refresh, enable/switch, disable, hook override, update, uninstall, details, and quit, but slash commands remain the canonical non-UI API. Short state-change confirmations can remain echo-area messages.

Shadowed duplicate plugin sources should stay visible in the management buffer below the winning plugin row. If activation still points at a shadowed source, the row should call that out and route the enable action through an explicit switch confirmation.

Plugin details are exposed from the management buffer, not as a separate slash command in this iteration. Pressing `RET` on a plugin row should show the consent/detail summary.

Plugin mutations should refresh the current session's visible plugin skills and hook state immediately when possible; activation should not require starting a new session.

The first implementation slice covers plugin and hook resource roots, source-bound activation state with hook consent fingerprints, global `.agents` installs with legacy `.mevedel` management, enable-all activation with consent, managed update/removal rules, and the `*mevedel plugins*` management buffer. Transient UI is deferred until the command API and buffer behavior settle.

Plugin install/update/removal remains source-specific: `/plugin install OWNER/REPO` writes to global `~/.agents/plugins/`, while legacy global `~/.mevedel/plugins/` installs remain manageable. `/plugin disable NAME` is the project deactivation path; `/plugin remove NAME` and `/plugin uninstall NAME` uninstall only global managed installs. If current workspace activation points at the removed source, it is cleared. Project-local plugins and extra roots are not updated or deleted by mevedel, and workspace plugin data is not deleted by uninstall.

Activation is bound to plugin name plus source root, not name alone. If a higher-precedence plugin with the same manifest name appears, it does not inherit enabled state from the shadowed source; plugin listing must report the conflict so the user can consciously switch to the new source.

Activation survives updates when the plugin name and source root remain unchanged, but executable hook consent is tied to the hook surface. If an update changes the hook files, events, commands, or functions, skills remain enabled and hooks require consent again before running.

Plugin runtime data remains workspace-scoped and keyed by plugin name rather than source root. Switching between roots for the same manifest name reuses the same plugin data directory.
