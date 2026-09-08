# Memory scope experiment: use shared notes by default

Run on 2026-09-08 using the configured gpt-5.6-sol, gpt-5.6-luna and
deepseek-v4-flash providers. Recommendation: write working notes directly to a
supplied `shared://sessions/<session>/notes.md` address. Keep managed plans under
`local://plans/`. Keep curated memory and historical journal conceptually
separate from working notes.

The split was understandable to all three models. Its weakness was the extra
decision about publishing: Luna correctly updated a local finding but did not
publish it in either trial. The next session therefore could not recover it.
Shared-by-default avoided that failure without making the models treat tentative
notes as established knowledge.

## Scope and results

The frozen [protocol](README.md) used three synthetic scenarios, two trials and
two policies per model. Each case ran a real writer followed by a fresh model
request with no writer conversation. Two cases crossed a session boundary; the
scratchpad case retained the local directory. This produced 36 writer/reader
pairs, or 72 requests. All requests completed; none timed out or needed a harness
retry. Invalid patches were returned to the model and could be repaired within
the same request.

| Model | Split: findings recovered | Shared-default: findings recovered | Split: cross-session recovery | Shared-default: cross-session recovery |
|---|---:|---:|---:|---:|
| gpt-5.6-sol | 6/6 | 6/6 | 4/4 | 4/4 |
| gpt-5.6-luna | 4/6 | 6/6 | 2/4 | 4/4 |
| deepseek-v4-flash | 6/6 | 6/6 | 4/4 | 4/4 |

These are factual retrieval scores, not a claim that every response perfectly
preserved every instruction. Every writer and reader response was inspected for
the supplied facts and qualifications. The only unavailable findings were
Luna's two split-policy correction cases. Both readers correctly acknowledged
missing evidence instead of inventing the answer.

All 12 scratchpad readers retained the untested status, absence of tests and
deterministic-seed next step. All explicit handoffs were discoverable. All
writers corrected the backend finding accurately in their own notes. Across
all 36 pairs, the managed plan, peer session's notes, memory file and journal
file remained byte-for-byte unchanged. There were no attempts to patch memory or
journal. This checks preservation of existing files, not concurrent editing.

## The decisive case

The writer was asked to update an existing investigation note: production is
PostgreSQL 16, MariaDB 11 is only a disposable staging fixture, PostgreSQL logical
replication is the selected export approach, and no migration or deployment
occurred. There was no explicit handoff request. A later session asked for these
findings.

In both Luna split trials, the correct update existed only in
`local://notes.md`. The new session saw only an unrelated peer note and could
not recover the correction. Luna followed the default location policy; it
missed the optional publishing decision. In both shared-default trials, its
update was already project-visible and the new session recovered all four facts.
Sol and Flash published the correction in both split trials.

This is evidence for removing an avoidable decision, rather than evidence that
four address names are intrinsically too complicated.

## Duplication and work

The writer wrote notes in both local and shared scopes in 4/6 Sol split cases,
2/6 Luna split cases and 3/6 Flash split cases. Shared-default writers used a
single shared note in every case. The local and shared versions were not always
identical copies. Readers sometimes created further summaries in both
policies, so shared-default alone does not solve repeated-summary accumulation.
The common system prompt encouraged persistence, including on reader requests;
that makes reader-copy counts a poor measure of spontaneous duplication.

Totals include writer and reader requests, searches, successful patches and
failed patch attempts. Median seconds cover a complete writer/reader pair.

| Model | Policy | Tool calls | Patch format errors | Input, uncached | Input, cached | Output tokens | Median seconds |
|---|---|---:|---:|---:|---:|---:|---:|
| Sol | Split | 82 | 1 | 38,205 | 0 | 6,804 | 57.2 |
| Sol | Shared-default | 79 | 2 | 38,037 | 0 | 6,160 | 54.2 |
| Luna | Split | 74 | 0 | 36,272 | 0 | 5,722 | 38.2 |
| Luna | Shared-default | 68 | 1 | 36,654 | 0 | 5,795 | 38.8 |
| Flash | Split | 127 | 7 | 15,916 | 121,856 | 19,904 | 29.5 |
| Flash | Shared-default | 75 | 5 | 13,141 | 69,376 | 10,664 | 13.9 |

Flash used materially fewer tools and output tokens with shared-default in this
sample. Sol's differences were small, and Luna's token and time differences were
negligible. Do not generalize these timings: the models ran concurrently, prefix
caching differed, verbosity differed, and patch-format repair affected totals.
Missing-file probes are recorded separately in the raw calls and are not counted
as patch errors or scope misunderstandings.

## Separate content-quality finding

Four handoff readers weakened the unconditional “do not deploy” constraint into
a conditional instruction about waiting for PostgreSQL validation: Sol in both
shared-default trials, Luna in split trial 1, and Flash in split trial 2. The
last explicitly added “Deploy only after PG confirms the same 8/8.” The original
handoff did not grant that authorization. All answered no to deploying now, but
the added condition changes the future instruction.

All published handoff files retained the prohibition. This is a reader summarization issue
seen with both policies and across all three models. It should be addressed in
handoff prompting and evaluated separately; changing the storage address does
not fix it. The retrieval table above deliberately does not hide it inside a
single overall quality score.

## Limits and implementation consequence

This was a small behavioral prototype with short prompts and few files. It did
not test crowded project directories, simultaneous edits, cross-project access,
automatic journal/consolidation integration or long-running agent conversations.
Memory and journal were read-only, matching today's ordinary tool access. Their
future writable behavior needs its own evaluation.

Both arms had identical tools and address capabilities. The difference was the
default notes location and whether publishing was a separate decision. The
adapter used real temporary files and mevedel's patch parser, matcher and
transaction, but it was not the production resource resolver. No production
`shared://` support or live model configuration was changed.

For implementation, offer one obvious working-notes destination under shared,
keep plans session-local, and preserve source references and uncertainty when
continuing someone else's note. Prefer referencing or updating an existing
finding over copying it into every session's notes. The existing journal and
learning paths that capture `local://notes.md` would need to follow the new
location as part of that implementation.

This result supports the simpler default. It does not establish a general model
ranking or a statistically reliable failure rate from two repetitions.

## Reproduction and evidence

Base implementation: `0a1e5a4`. Experiment branch: `memory-scope-evaluation`.
Emacs 31.1; installed gptel `20260906.334`, gptel-agent `20260824.106`.
Codex models used reasoning effort `none`; Flash used its default. Backend
snapshots came from the running configuration, were mode 0600, and were deleted
after loading. Only synthetic evidence was sent to providers.

- [Harness](run.el), [protocol and commands](README.md), [measurement renderer](summarize.py).
- Raw local evidence: `.scratch/memory-scope-evaluation/{sol,luna,flash}.json`.
  Each contains exact system/task prompts, tool arguments/results, final replies,
  writer file snapshots, token counts and elapsed times.
- Expanded local review sheet: `.scratch/memory-scope-evaluation/review.md`.

After the recorded provider run, only the runner's progress printing was removed
to keep future ERT output quiet. The prompts, cases, tools and measurement logic
are unchanged. The local adapter smoke test was rerun after that edit.
