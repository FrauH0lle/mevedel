# Memory scope experiment (throwaway prototype)

Question: does keeping session notes in `local://` and selectively publishing
them to `shared://` cost reliability compared with writing working notes directly
to `shared://sessions/<session>/notes.md`?

This is a behavioral experiment, not a production implementation of `shared://`.
Both arms expose identical Read, Glob, Grep and ApplyPatch tools. The adapter maps
addresses onto private temporary directories and uses mevedel's real patch
parser, matcher and transaction. Memory and journal remain read-only, matching
today's ordinary tools. No messages, real project data or live configuration are
changed. All evidence is synthetic.

## Frozen protocol

Models: configured gpt-5.6-sol, gpt-5.6-luna, deepseek-v4-flash. Codex effort
`none`; DeepSeek uses its default. Compare policies within each model; these are
not equal-compute comparisons between model families.

Two trials, three scenarios, both policies. Each writer is followed by a separate
request with no writer transcript. Scratchpad readers retain the session's local
directory; handoff and correction readers receive a new empty local directory.
Shared files, memory and journal remain project-wide. Alternate arm order by
trial. No automatic retries of unsuccessful samples. Maximum 12 provider rounds
and 180 seconds per request. Record raw replies, tool calls/results, file states,
input/cache/output tokens, elapsed time and transport failures.

- Scratchpad: preserve an explicitly unverified hypothesis for this session;
  keep the managed plan intact and leave another session's notes intact.
- Handoff: preserve observed SQLite coverage, the untested PostgreSQL backend
  and the no-deployment constraint for a colleague in another session.
- Correction: replace a seeded incorrect MariaDB assumption with observed
  PostgreSQL configuration and a staging-only exception. The writer is asked to
  update notes, without an explicit handoff instruction; later another session
  needs the finding. This tests the cost of deciding what to publish.

Score semantic recovery from reader responses against the supplied facts, plus
successful persistence, correct destination under the assigned policy, no
accidental memory promotion, preserved unrelated files/plans, stale claims and
duplicate notes. Policy compliance and cross-session usefulness are separate:
keeping a correction local may follow the split policy but lose the handoff.
Automated indicators assist an explicit reading of every response. Two trials
are a small diagnostic sample, not a statistically conclusive model ranking.

## Run

Export a private provider snapshot with the existing
`test/manual/memory-quality/export-provider.el` from the running Emacs, setting
`MEVEDEL_QUALITY_BACKEND` to Codex or DeepSeek. Pass its returned pathname only:

```sh
npx @emacs-eask/cli clean elc
MEVEDEL_SCOPE_PROVIDER=/tmp/PRIVATE-CONFIG.el \
MEVEDEL_SCOPE_MODEL=gpt-5.6-sol \
MEVEDEL_SCOPE_OUTPUT=/tmp/scope-sol.json \
npx @emacs-eask/cli test ert test/manual/memory-scope/run.el
```

The runner deletes its provider snapshot. It saves a JSON checkpoint after every
case. `MEVEDEL_SCOPE_SMOKE=1` runs only local adapter checks and needs no provider.
Do not run this file via an ordinary wildcard or retain credentials in results.

Render the measurements and review sheet from the directory of model JSON files:

```sh
python3 test/manual/memory-scope/summarize.py .scratch/memory-scope-evaluation
```

`input_tokens` is gptel's uncached input count; add `cached_tokens` for total
input. The ERT result indicates harness completion. Semantic recovery is reviewed
separately in [RESULTS.md](RESULTS.md); a successful provider request can still
fail to recover a finding.

## Follow-up: nested shared address

Set `MEVEDEL_SCOPE_NESTED=1` to run only the third arm, `nested-shared`.
It uses the same three cases, two trials, tools, facts, limits and model settings.
Project notes live at `local://shared/sessions/<session>/notes.md`; plans and other
local files stay session-owned. Searching `local://` includes the visible shared
subtree. `shared://` is rejected, not retained as an alias. No changes are made to
the earlier raw results. Write the new JSON files to a separate output directory.

This adds 36 real requests across the same three models. Compare against the
earlier shared-default arm as a historical control; there is no simultaneous
rerun of that control. Review retrieval, uncertainty, protected-file preservation,
scope mistakes and conditional deployment drift separately, using the same
criteria. Timing comparisons across the two runs remain descriptive only.

Results: [nested schema follow-up](NESTED-RESULTS.md).

## Follow-up: agent-chosen organization

Set `MEVEDEL_SCOPE_FREEFORM=1` to run `freeform-work`: the same three scenarios
and two trials, now using only `work://`, `memory://` and `journal://`. This adds
36 requests across Sol, Luna and Flash. No notes filename or directory layout is
supplied. Guidance says to put notes in `work://shared/`, search before creating,
update relevant existing material, and organize as needed. The unrelated peer
note starts at `work://shared/ui-investigation.md`. Only the correction scenario
has an existing relevant note: `export-investigation.md` in trial 1 and
`backend-findings.md` in trial 2. The model must discover its path through tools.

Freeze the same factual recovery and constraint-preservation criteria, and also
inspect whether writers search first, choose a shared destination, and update
the seeded correction instead of leaving competing stale notes. Record final
shared files after the reader as well, to expose duplicate summaries. Directory
creation by the model is permitted, not a failure. Keep raw results in a new
directory. Earlier arms are historical controls; changing both spelling and note
organization means this tests the proposed workflow, not a causal naming effect.

Results: [agent-organized shared notes](FREEFORM-RESULTS.md).
