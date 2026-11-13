# The Note Frame Is A Child Frame, Not An Overlay

Status: accepted

## Current decision

A note's answer should be readable and actionable without the session view
taking over the window layout, so a note opens a floating child frame at
its source position showing its own conversation session's view. The frame
displays the real view buffer, so permissions, Ask, patch review, streaming,
and the composer work in it unmodified and no second renderer exists. It takes
focus when opened explicitly from the note's actions and stays unfocused
when a dispatch opens it, since a request the user just started should not move
point. Terminals and unavailable child frames use a side window.

`mevedel-note-frame.el` owns this presentation; `mevedel-note-source.el`
coordinates source anchors and durable note records. Frame options and symbols
use note names, with no aliases for the retired directive names.

The frame displays the whole note session, including a batch's shared history.
There is no per-directive turn filter or shared-chat composer scope. Its tab line
shows note identity and state without taking the pinned prompt's header line.

## Rationale and consequences

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

[0105](0105-cockpit-surfaces-follow-three-archetypes.md) does not apply. The
note frame is a display geometry for the primary transcript surface, not a
cockpit surface, and it adds no fourth archetype. The shared `q`/`?`/`g` key
contract is independently impossible here: the view buffer contains an editable
composer, so single-letter bindings would break typing. Prefixed note actions
leave ordinary characters to the composer. Closing preserves the conversation
and draft and restores the invoking source position.

## Decision history

The directive frame originally displayed a bound execution session and filtered
its transcript to one directive only while it was the view's sole window.
Invisibility belonged to the buffer, not the frame; an indirect buffer would
lose interaction overlays, while a separately rendered copy would duplicate
renderers and interaction ownership. Opening entered a sticky directive composer
scope; closing cleared it to protect ordinary-chat follow-ups.

[ADR 0132](0132-notes-replace-references-directives-and-buddy-notes.md) replaces
that binding, filter and scope with independent note sessions. Their ordinary
view already contains exactly the conversation to display, so those mechanisms
and the filter toggle are removed. Child-frame geometry and the point-addressed
interaction rationale remain; the module and option rename reflects that owner.
