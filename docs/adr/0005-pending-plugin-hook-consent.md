# Pending Plugin Hook Consent Interrupts Interactive Startup

Executable plugin hook consent is part of the plugin trust boundary, not just
plugin metadata. When an enabled plugin's hook surface changes, mevedel keeps
plugin skills active but withholds executable hooks until the user reviews the
changed surface.

Interactive session startup or resume actively notifies the user when any
enabled plugin has pending hook consent. Because withheld hooks can make an
enabled plugin behave incorrectly, mevedel reports this as a warning/message
and routes review through the plugin cockpit. Consent is reviewed one plugin
at a time; mevedel does not offer a blanket approve-all action for multiple
executable hook surfaces.

Until the user approves the changed hook surface, the session continues with
those hooks withheld and the plugin cockpit continues to show the pending
consent state. Noninteractive or batch contexts should not block; they should
report the pending consent through user-visible warnings/messages instead.

Pending consent is user configuration and security state, not model context.
It should be shown in the user-facing session/view surfaces and messages, not
injected as a model-visible reminder.
