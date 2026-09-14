# Buddy notes are not instructions

Status: accepted

## Current decision

Buddy notes are model-authored annotations attached to a source line. They are
overlays in a source buffer, which makes them look like a third instruction
flavour beside references and directives. They are not one, and they must not
become one.

An instruction is user-authored and source-linked: a reference contributes
context, a directive asks for work and owns durable activity. A note is neither.
It is not user-authored, asks for no work, and it survives no restart. It is pure
presentation with no durable identity at all — which is the same distinction
ADR 0087 draws when it keeps directive identity outside the source overlay,
taken to its limit.

The mechanism follows from that. Instruction enumeration selects overlays
carrying the `mevedel-instruction` property; navigation, tinting, priority,
persistence, deletion, and subdirective resolution all key off it. A note that
never sets that property is therefore invisible to every one of those paths
without any of them being modified, and without a third value being added to
`mevedel-instruction-type`.

## Rationale and alternatives

The alternative — notes as instructions with an ephemeral flag — was rejected
because it inverts the burden. Every existing instruction code path would have
to learn to skip them, and each new one would have to remember to. Under this
decision a path only sees notes if it deliberately asks for the note property.

## Consequences

- Note overlays must never set `mevedel-instruction`. This is the whole
  contract; a single `overlay-put` would silently enrol notes in instruction
  navigation, tinting, and workspace persistence.
- Notes cannot be persisted by the directive record codec, cannot be rewound,
  and do not appear in the directive activity surface. All three are intended:
  notes are re-derived from current buffer state rather than restored.
- Buddy owns note lifecycle end to end, including dropping a killed buffer's
  notes, because no instruction machinery will do it on Buddy's behalf.

## Decision history

The original design considered a third instruction type and an audit of every
binary reference/directive assumption. Reading the enumeration predicate showed
that omitting the instruction marker property provides structural separation.
An ephemeral flag on instructions would instead require every present and future
caller to remember to skip notes.

Buddy was ported from [llm-buddy](https://github.com/ahyatt/llm-buddy). The port
avoided adding its `llm` provider abstraction beside gptel. At the time of that
comparison, the upstream forced-tool loop needed an explicit end tool, and its
buffer read/note placement lacked mevedel's bounded buffer scope and captured
markers. Mevedel chose ordinary no-more-tools settlement, request-local tools,
workspace scope from default-directory, and bounded marker-based reads/notes.
Those observations describe the port's motivation, not a claim about every
current upstream release.

The upstream replace-content auto-fix path was not ported because mevedel already
has permission-gated patch proposals and review. Passive annotations also do not
write durable memory: the removed Tutor experiment had accumulated an unread
hints file indefinitely. Durable guidance uses explicit instruction/memory
workflows; Tutor removal is recorded in
[ADR 0070](0070-compose-system-prompts-from-ordered-profiles.md#decision-history).
