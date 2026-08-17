# The Directive Frame Is A Child Frame, Not An Overlay

A directive's answer should be readable and actionable without the session view
taking over the window layout, so a directive opens a floating child frame at
its source position showing its bound execution session's view. The frame
displays the real view buffer, so permissions, Ask, patch review, streaming,
and the composer work in it unmodified and no second renderer exists. It takes
focus when opened explicitly from the directive's actions and stays unfocused
when a dispatch opens it, since a request the user just started should not move
point.

An overlay `after-string` was the obvious cheaper surface and cannot work.
Interactions are point-addressed: `mevedel--prompt--overlay-at-point` resolves
its target through `(point)`, and the approve/deny/feedback keymap is an
overlay keymap that is live only while point is inside the overlay. Point
cannot enter a display string, so an overlay surface fails three ways at once —
the keymap never activates, the lookup returns nil even with globally bound
keys, and there is no position from which to settle the callback. Making it
work would mean re-addressing the interaction layer from point to identity,
which is a larger change than the surface is worth. Reading a response never
needed point, so a read-only peek would have worked; it is omitted because the
frame subsumes it, not because it was impossible.

Filtering the frame to one directive's turns uses `invisible` text properties
and the buffer's invisibility spec, which makes the filter a property of the
buffer rather than of the frame. An indirect buffer would give each display its
own spec over shared text, but overlays are not shared between an indirect
buffer and its base, so every interaction overlay would stay behind and the
frame would show no prompts — the same failure as the overlay, reached from the
other direction. Rendering a filtered copy into its own buffer is the hybrid
[0091](0091-render-directive-turns-in-the-shared-session-view.md) already
rejected for producing two renderers and two interaction owners. The frame
therefore filters only while it is the sole window showing that view, and
otherwise displays the full transcript positioned at the directive's turn.

[0105](0105-cockpit-surfaces-follow-three-archetypes.md) does not apply. The
directive frame is a display geometry for the primary transcript surface, not a
cockpit surface, and it adds no fourth archetype. The shared `q`/`?`/`g` key
contract is independently impossible here: the view buffer contains an editable
composer, so single-letter bindings would break typing. The frame binds only a
dismiss and a filter toggle, both prefixed, and leaves the rest of the keymap to
the view. Opening the frame enters the directive's composer scope and closing it
leaves that scope, so a later ordinary-chat message cannot silently become a
directive follow-up.
