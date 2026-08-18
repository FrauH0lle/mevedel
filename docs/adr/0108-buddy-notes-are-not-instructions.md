# Buddy notes are not instructions

Status: accepted

Buddy notes are model-authored annotations attached to a source line. They are
overlays in a source buffer, which makes them look like a third instruction
flavour beside references and directives. They are not one, and they must not
become one.

An instruction is user-authored and source-linked: a reference contributes
context, a directive asks for work and owns durable activity. A note is neither.
Nobody authored it, it asks for nothing, and it survives no restart. It is pure
presentation with no durable identity at all — which is the same distinction
ADR 0087 draws when it keeps directive identity outside the source overlay,
taken to its limit.

The mechanism follows from that. Instruction enumeration selects overlays
carrying the `mevedel-instruction` property; navigation, tinting, priority,
persistence, deletion, and subdirective resolution all key off it. A note that
never sets that property is therefore invisible to every one of those paths
without any of them being modified, and without a third value being added to
`mevedel-instruction-type`.

What moved this from a preference to a decision was reading the enumeration
predicate rather than assuming. The initial plan was a third instruction type,
and the estimate for it included auditing the render and persistence paths for
binary reference/directive assumptions. The predicate turned out to be a single
marker property, so the separation is free and structural: omitting one
`overlay-put` guarantees isolation that an audit could only have argued for.

The alternative — notes as instructions with an ephemeral flag — was rejected
because it inverts the burden. Every existing instruction code path would have
to learn to skip them, and each new one would have to remember to. Under this
decision a path only sees notes if it deliberately asks for the note property.

Consequences:

- Note overlays must never set `mevedel-instruction`. This is the whole
  contract; a single `overlay-put` would silently enrol notes in instruction
  navigation, tinting, and workspace persistence.
- Notes cannot be persisted by the directive record codec, cannot be rewound,
  and do not appear in the directive activity surface. All three are intended:
  notes are re-derived from current buffer state rather than restored.
- Buddy owns note lifecycle end to end, including dropping a killed buffer's
  notes, because no instruction machinery will do it on Buddy's behalf.
