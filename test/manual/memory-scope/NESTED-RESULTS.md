# Nested shared address: viable, with explicit scope guidance

The follow-up on 2026-09-08 tested `local://shared/...` with gpt-5.6-sol,
gpt-5.6-luna and deepseek-v4-flash. All three recovered the required findings in
all six cases. This supports using the proposed layout for working notes. It
does not show that nesting the address improves model understanding over a
separate `shared://` address.

| Model | Earlier split policy | Earlier shared-default | New local://shared/ default |
|---|---:|---:|---:|
| Sol | 6/6 | 6/6 | 6/6 |
| Luna | 4/6 | 6/6 | 6/6 |
| DeepSeek Flash | 6/6 | 6/6 | 6/6 |

Scores count factual recovery, not perfect prose or instruction preservation.
Each column has two scratchpad trials, two explicit cross-session handoffs and
two cross-session correction recoveries. All new cross-session cases passed:
4/4 per model. Luna's two missed corrections under the earlier split policy
were both recoverable under the nested schema.

## Controlled follow-up

The same models, reasoning settings, tool functions, three scenarios, two
repetitions, timeouts and request-round limits were retained. This added 18
writer/reader pairs, or 36 requests. Every request completed successfully, with
no timeouts or harness retries. The reader never received the writer's
conversation. All writer and reader replies were inspected.

Only the scope guidance and project address changed. Notes were supplied at
`local://shared/sessions/<session>/notes.md`. The `shared/` subtree resolved to
project storage, while `local://plans/` and other local files stayed
session-owned. `Glob`/`Grep` at `local://` included the shared subtree. The old
`shared://` address was rejected, so it could not serve as an unnoticed fallback.
No model attempted to use it.

All 18 pairs' task prompts and starting files were checked against the previous
shared-default arm: they matched exactly after normalizing the address prefix.
The earlier results were retained unchanged as a historical control; that arm
was not rerun concurrently. Model variability and provider conditions therefore
remain possible explanations for differences between runs.

## Preservation and scope understanding

All writers put their working note in the supplied project-owned location.
All scratchpad readers retained its untested status and next step. The original
plan, other session's note, memory file and journal file remained byte-for-byte
unchanged in every case. There were no memory/journal write attempts.

Flash nevertheless gave two questionable scope explanations:

- In handoff trial 1, it called the absence of the previous session's managed
  plan a discrepancy to flag. That absence was expected because the new session
  received an empty local directory. Sol correctly acknowledged that the old
  plan was not visible without treating it as a fault.
- In correction trial 2, it claimed `config/production.env` belonged to the
  previous session's inaccessible local scope. The evidence established only
  that the config file was unavailable to the reader, not its ownership or
  storage location. The recovered database facts themselves were correct.

These explanations are reasons to make the two ownership rules explicit. The
sample is too small to attribute them confidently to the nested spelling.

## Other quality observations

Both Flash handoff readers changed the unconditional no-deployment instruction
into wording that gated deployment on PostgreSQL validation. All six handoff
readers answered no to deployment now. Sol and Luna did not introduce that
specific conditional permission in this follow-up. As in the earlier test,
retrieval success does not guarantee faithful preservation of instructions.

Flash's first scratchpad file also contained a stray `--- Begin Patch` line and
an invented placeholder date. The reader noticed the malformed header and still
recovered the hypothesis correctly. This was accepted file content, separate
from the patch parser errors counted below.

## Work performed

Each row totals six writer/reader pairs. The earlier comparison uses the
shared-default arm only. Token counts include all provider rounds; input is
shown separately for cached and uncached tokens, following gptel's accounting.

| Model | Address | Tool calls | Patch format errors | Input, uncached | Input, cached | Output tokens | Median pair seconds |
|---|---|---:|---:|---:|---:|---:|---:|
| Sol | shared:// | 79 | 2 | 38,037 | 0 | 6,160 | 54.2 |
| Sol | local://shared/ | 69 | 1 | 36,758 | 0 | 5,876 | 52.5 |
| Luna | shared:// | 68 | 1 | 36,654 | 0 | 5,795 | 38.8 |
| Luna | local://shared/ | 72 | 0 | 39,250 | 0 | 5,736 | 33.1 |
| Flash | shared:// | 75 | 5 | 13,141 | 69,376 | 10,664 | 13.9 |
| Flash | local://shared/ | 90 | 3 | 11,291 | 88,448 | 13,560 | 20.9 |

There is no consistent efficiency winner. Search choices, repeated summaries,
format repairs, caching and timing variability affect these small samples.

## Decision and limits

Use this layout if the preference is one address family for working files:

- `local://shared/...`: project-owned working notes, visible across sessions.
- `local://plans/...`: current-session managed plans.
- `memory://`: curated guidance; `journal://`: historical evidence.

Provide the model's exact notes address and state the ownership distinction.
The important improvement over the split policy remains writing notes directly
to project-visible storage, without a separate publishing decision.

This is an isolated behavioral prototype, not production resource support. It
does not validate session cleanup, forking, concurrent writers, large project
directories, remote storage, cross-project access or writable memory. Those
lifecycle properties would need implementation-level verification. No production
configuration or resource behavior changed.

## Evidence and validation

See the [protocol](README.md), [runner](run.el), [measurement renderer](summarize.py)
and [earlier results](RESULTS.md). The same installed dependencies and model
settings as the earlier run were used. Temporary provider snapshots were deleted.

Local raw evidence and the expanded review sheet are retained at:

- [Sol](../../../.scratch/memory-scope-nested/sol.json)
- [Luna](../../../.scratch/memory-scope-nested/luna.json)
- [Flash](../../../.scratch/memory-scope-nested/flash.json)
- [Review sheet](../../../.scratch/memory-scope-nested/review.md)

The adapter smoke test checks cross-session shared reads and updates, isolation
of ordinary local notes, root search visibility, rejection of the old scheme
and rejection of traversal. Source compilation and whitespace checks also pass.
