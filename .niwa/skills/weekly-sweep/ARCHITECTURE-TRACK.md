# Architecture track

Runs the attached improve-codebase-architecture skill unattended. Its files,
including `HTML-REPORT.md`, are at
`skill://global-agents/improve-codebase-architecture/`; agents read them there,
since that skill is user-only and cannot be invoked. Follow it with these
overrides. This repository's glossary is `CONTEXT.md` (the skill's
`GLOSSARY.md`; see `docs/agents/domain.md`).

## Overrides

- **Scope** (skill step 1): take the intake's focus areas when given;
  otherwise the skill's hot-spot walk, weighted also by backlog areas and
  test-track evidence (slow or brittle tests from a hard-to-reach seam).
- **Report** (skill step 2): write it without opening it, record the path in
  the ledger and `overview.md`, and do not ask which candidate to explore.
- **Grilling order** (skill step 3): Strong candidates first, the top
  recommendation first among them. The user's seat is taken by the proxy user
  and second opinion below.
- **ADR offers**: where the skill offers the user an ADR, record the rejected
  candidate in the relevant ADR's rejected alternatives (or a new ADR) and list
  it under "Review afterwards"; skip ephemeral and self-evident reasons.

## Workstreams

1. **Explore** (a fresh strong-tier agent): skill steps 1 and 2, briefed with
   the skill's address and the overrides.
   Done when every candidate has all card fields and the report path is in
   the ledger.
2. **Grill** each candidate. Three coordinator children message each other
   directly with explicit targets; spawn the proxy and the second opinion
   first so the questioner's brief can name their paths.
   - The **questioner** (strong tier, grilling skill) first checks the
     candidate against [DECISION-RULES.md](DECISION-RULES.md). A candidate a
     rule already rejects is recorded as rejected with that rule, without
     questions. Otherwise it grills per skill step 3: at most five questions
     per round, six rounds per candidate.
   - The **proxy user** (the partner model, briefed with
     [PROXY-USER.md](PROXY-USER.md)) answers each question.
   - The **second opinion** (strong tier from the preset, so a different
     model family than the proxy; briefed with PROXY-USER.md) answers only
     the questions the proxy marked low confidence, without seeing the proxy's
     answer. When both agree, the decision stands with high confidence. When
     they differ, the questioner takes the more reversible option, marks it
     **disputed**, and the grilling continues.
   The questioner appends every question, both answers, the rule or source
   and the confidence to `decisions.md`, keeps the domain model current on
   the candidate's branch as the skill's side effects describe, and forwards
   `NEEDS USER` answers to the coordinator. Per settled candidate it writes
   `arch-brief-N.md` beside the ledger and notifies the coordinator, which
   dispatches workstream 3.
   Done when the candidate's deepened module shape is settled, or its blocking
   question is in `needs-user.md`.
3. **Implement** each settled candidate on its own branch, test through the
   deepened module's interface, and review it like any other change. When an
   open decision blocks the module's shape, record it and move to the next
   candidate.
   Done when every Strong candidate is implemented on a branch or blocked by a
   decision recorded in `needs-user.md`.
