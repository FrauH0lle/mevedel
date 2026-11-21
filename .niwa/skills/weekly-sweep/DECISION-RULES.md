# Decision rules

The user's settled design judgement, distilled from sweep decisions they did
not reverse. The proxy user applies these first and cites the rule it used; the
questioner checks each candidate against them before asking. A rule settles a
question with high confidence. When none applies, use the tie-breaks.

Maintained by the sweep: the close adds disputed decisions to "Unsettled";
the intake asks the user about them, folds each answer into a rule, sharpens a
rule they corrected, and deletes one they overruled.

## Rules

1. **One deep owner.** Put behaviour behind the existing owner's small
   interface; keep substeps private and give callers statuses, not internals.
   *Source: CODING_STANDARDS DEEP HARNESS; 2026-10-31 candidates 1, 2, 4.*
2. **Fresh guards at the consequential write.** Reuse a proof only inside one
   continuously owned phase; re-check ownership, bytes, deadline and live use
   at each write. *Source: candidates 1 and 5.*
3. **Real producers in tests.** Speed tests up with real-clock offsets,
   barrier files, captured timer callbacks or smaller real fixtures; keep the
   real producer, parser and carrier. *Source: docs/development.md testing
   conventions; candidate 6, clusters C04, C05, C10, C16.*
4. **Negative control for every claim.** Show that the injection is reached
   and that a discriminating control fails without the fix. *Source: memory
   controlled-test-attribution, reentrant-view-render-ordering.*
5. **Assertion strength is fixed.** Fix the cause; skip visibly when a
   precondition is missing. *Source: clusters C12, C17, T3.*
6. **Delete a test only with its assertions transferred.** Name the tests that
   now cover each assertion; keep unique safety assertions until public
   coverage replaces them. *Source: candidates 1, 2; test-track deletions.*
7. **Matched measurements decide.** Numeric targets are provisional goals;
   accept only repeated, compiled, same-load measurements and report null
   results. Thresholds such as programs-per-decision guards are provisional
   goals, not hard gates. *Source: memory view-render-performance-constraint;
   candidates 1, 5; the user at the 2026-11-07 intake.*
8. **Stop at the smallest verified gain.** Drop an optimisation that shows no
   benefit rather than add machinery to rescue it. *Source: CODING_STANDARDS
   PROPORTIONATE SOLUTIONS; C16, candidate 4.*
9. **User features stay.** Keep every capability; show an unavailable control
   honestly. Removing one is an escalation. *Source: memory
   development-priorities-user-preference; candidate 5.*
10. **Abstractions wait for evidence.** Reject a cross-cutting port, registry,
    adapter or second implementation until measurements justify it; keep
    central review points such as the privacy allowlist. *Source: ADR 0101,
    ADR 0128; candidates 5, 6, 7.*
11. **Reproduce before fixing or closing.** Write the reproduction first; keep
    an unreproduced lead open unless later rewrites removed its code.
    *Source: memory controlled-test-attribution; candidates 3, 4.*
12. **Amend in place.** Update the existing ADR, glossary entry or doc where
    its rule changes; tracked docs never cite shared notes. *Source:
    CODING_STANDARDS ADRS ARE REVISABLE; the user at the 2026-11-13 intake
    (warm-admission refusals stay in the amended "portable project
    authority" glossary entry, no separate term).*
13. **Reuse existing mechanisms.** Prefer an existing primitive, a
    buffer-local override or derivation from existing data; add no new state,
    migration or alias. Instrument internal promises with test-only helpers,
    not a production debug mode. *Source: CODING_STANDARDS NO BACKWARDS
    COMPATIBILITY; the user at the 2026-11-07 intake.*
14. **Fix the owner of a load-order gap.** Repair cold-start or load-order
    gaps in the production module that owns them, with a fresh-Emacs
    regression. *Source: memory verify-compiled-runtime-artifacts; T4, C07.*
15. **Point, composer and windows stay put.** Background work moves none of
    them. *Source: CODING_STANDARDS NO SURPRISES; ADR 0126; candidate 4.*
16. **Bounded inventories over scanners.** Enforce a writer or caller
    inventory with a bounded list plus behavioural tests, not a syntactic
    scan. *Source: the user at the 2026-11-07 intake.*
17. **Batch first, then a long-lived helper.** For subprocess cost, batch
    operations through the existing carrier first; a separately reviewed
    long-lived helper process is acceptable when batching gets too
    complicated or does not achieve the gains. *Source: the user, 2026-11-07
    (needs-user #1).*
18. **Honest terminal wording.** A state nobody observed reads as what is
    known ("lost", "ended"), never as "failed" or "completed". *Source:
    the user, 2026-11-07 (candidate 9 dispute).*
19. **One implementation per concern.** When one mechanism can serve two
    cases, keep only that one; a second protocol, profile or path beside it
    needs a clear, stated justification (a measured loss the single one cannot
    fix). A replacement replaces; it does not add a parallel path. *Source:
    the user, 2026-11-14 (session locking, memory-topic writes).*

## Tie-breaks

- Lower complexity and better performance.
- The narrow, per-case change over a generic one.
- The smaller caller-visible surface (public verbs over a tagged request).
- A smaller real fixture over a new seam.
- Merge a reviewed green tip now rather than hold it.

## Unsettled

Ask the user at the next intake, then fold the answer into a rule.
