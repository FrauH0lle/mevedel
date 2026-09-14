# Domain documentation

The repository uses one domain context: the root `CONTEXT.md` defines terms, and
`docs/adr/` records architectural choices and their rationale. The
[documentation map](../index.md#documentation-map) identifies current behavioral
contracts; the [ADR index](../adr/README.md) maps original decision IDs to their
canonical locations.

## Using the vocabulary

Read the relevant glossary entries and decisions before exploring an unfamiliar
area. Use the established terms in issues, code, tests, and explanations. If a
concept is missing or ambiguous, identify the gap and resolve its meaning before
adding competing terminology. Update `CONTEXT.md` when an implemented concept
needs a definition; exploratory notes belong under `.scratch/`.

## Revising decisions

An accepted ADR records a decision's reasons, not a prohibition on changing it.
When implementation evidence conflicts with a record, establish the current
behavior and explain the discrepancy. When behavior changes, update its manual
contract and ADR in the same change. Preserve the previous choice, replacement,
and evidence that moved the decision in its history section.

Keep one canonical record per coherent decision. Fold amendments into its current
explanation, preserve independent decisions, and map absorbed IDs in the ADR
index. Repository entry rules are in `AGENTS.md`; detailed ADR maintenance rules
are in the [ADR index](../adr/README.md#maintaining-decisions).
