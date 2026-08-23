# Request config lives in the sidecar, not Org properties

Status: accepted

The only top-level gptel Org property mevedel transcripts may carry is
`GPTEL_BOUNDS`; a transcript with no persisted bounds carries none.
Every other `GPTEL_*` property — backend, model, reasoning effort, preset,
system prompt, tools, temperature, token limits — is stripped from the
top-level drawer at save time, at chat-buffer init, and at agent hydration
(`mevedel-session-artifacts-strip-gptel-config-properties`). The session
sidecar (`:model-provider`, `:reasoning-effort`, `:preset-name`) and the
retained-agent registry are the only mevedel-managed durable sources of
request configuration, and the buffer-locals rebuilt from them are the
canonical live source.

What forced the decision: gptel advises `gptel-send` with
`gptel-org--send-with-props`, which in an org buffer prefers the drawer's
properties over the buffer-local values for the request. Mevedel's
failed-turn autosave wrote those properties from the buffer-locals current
at save time, so a 401 turn pinned the failing backend into the drawer;
the user then switched models through the cockpit — which correctly
updated the buffer-locals and the sidecar — and the next send silently
went to the old backend anyway, because the stale drawer outranked both.
The freshly chosen reasoning effort leaked through in the same request
only because no effort property existed yet, which is what identified the
mechanism. The failure healed itself one failed turn later when the next
autosave rewrote the drawer, making the bug look like "changing the model
needs two attempts".

The alternative — keeping the drawer in sync by writing it from
`mevedel-model-set-session-provider` and every other config mutation —
was rejected: it preserves two sources of truth whose divergence is
silent and only observable as a wrong provider on the wire. Removing one
source is strictly smaller. Storing config once, in the sidecar, was
already the design (`mevedel-model-apply-session-policy` and
`mevedel-preset-restore-session` rebuild buffer-locals on init); the
drawer copies were redundant the whole time.

Consequences: gptel's top-level persisted request configuration is absent,
so it cannot override the live buffer-locals. Resume paths enable
`gptel-mode` with `gptel--restore-state` disabled — mevedel restores
`GPTEL_BOUNDS` itself — which avoids duplicate bounds restoration and the
"Could not activate gptel backend" noise a config-free drawer would produce.
The strip
matches by `GPTEL_` prefix with an explicit `GPTEL_BOUNDS` exemption, so
a config property added by a future gptel is stripped automatically
instead of reintroducing the override. Segments written before this
decision still carry the properties; they are deleted on sight at init
and on first save, with no compatibility reader.
