# Agent-organized shared notes: keep the directory unstructured

The 2026-09-08 follow-up supports leaving `work://shared/` unstructured by
default. Sol, Luna and DeepSeek Flash chose usable filenames, found existing
notes, updated the relevant correction and recovered the findings without a
prescribed notes path or directory layout.

| Model | Findings recovered | Writers searched first | Existing corrections updated | Cases with competing note files |
|---|---:|---:|---:|---:|
| gpt-5.6-sol | 6/6 | 6/6 | 2/2 | 0/6 |
| gpt-5.6-luna | 6/6 | 6/6 | 2/2 | 0/6 |
| deepseek-v4-flash | 6/6 | 6/6 | 2/2 | 0/6 |

Each model ran two scratchpad trials, two explicit cross-session handoffs and
two cross-session correction recoveries: 18 writer/reader pairs and 36 requests
overall. All requests completed without a timeout or harness retry. All writer
and reader replies were inspected. As in earlier reports, factual recovery is
separate from faithful preservation of every instruction.

## What changed

The model received no exact notes filename and no required subfolders. The
working-note guidance was:

> Put working notes in work://shared/. Search for relevant existing material
> before creating a file; update it when appropriate. Choose filenames and
> organize the shared directory as needed. Avoid unnecessary duplicate copies.

The same three synthetic tasks and two trials were retained. The unrelated
peer note began at `work://shared/ui-investigation.md`. In the correction case,
the existing relevant note was `export-investigation.md` in trial 1 and
`backend-findings.md` in trial 2. Neither path was given in the task prompt;
models had to discover it through tools. The new session had an empty private
working directory and no writer conversation, but could see shared files.

The protocol is recorded in [README.md](README.md). Earlier results are
historical controls. Both the address spelling and organizational guidance
changed, so this evaluates the proposed workflow, not the isolated causal
effect of either change.

## Organization and preservation

Every writer issued Glob or Grep before its first patch. New notes received
descriptive filenames such as `kite-217-investigation.md`,
`kite-217-retry-jitter.md` and `KITE-318-handoff.md`. Models chose files directly
under `work://shared/`; the harness did not require that choice or forbid
subdirectories.

All six correction writers updated the existing file in place. Every case had
exactly one relevant shared note after the writer and after the reader. No
reader created a competing copy. All readers retained the scratchpad's untested
status and next step. All 12 cross-session findings were recovered.

The managed plan, peer's note, memory file and journal file stayed byte-for-byte
unchanged in all 18 pairs. There were no attempts to use the old local/shared
schemes or patch memory/journal.

Readers sometimes rewrote or appended to the existing note: Sol in 6/6 cases,
Luna in 5/6 and Flash in 1/6. Thus, no duplicate files does not mean no repeated
prose. The common system prompt encouraged persistence on reader requests too;
that limits what can be inferred about spontaneous reader edits. The useful
observation is that the models reused the existing note instead of opening a
second one.

## Separate content-quality issue

In handoff trial 1, both Sol and Luna weakened the unconditional “do not deploy”
instruction into wording permitting reconsideration after PostgreSQL testing.
All handoff readers answered no to deploying now. This repeats the independent
instruction-preservation issue seen in earlier arms; successful file organization
does not solve it. It is not counted as a failed factual retrieval.

## Measurements

Totals include both phases and all provider rounds. Input follows gptel's
separate uncached/cached accounting. Patch errors include model-repairable
format or hunk-matching failures, not failed transport requests.

| Model | Tool calls | Patch errors | Input, uncached | Input, cached | Output tokens | Median pair seconds |
|---|---:|---:|---:|---:|---:|---:|
| Sol | 66 | 0 | 33,718 | 0 | 5,497 | 49.9 |
| Luna | 70 | 2 | 44,341 | 0 | 6,068 | 32.2 |
| Flash | 76 | 1 | 11,836 | 65,280 | 10,699 | 15.7 |

These small samples and separate run times do not establish a model ranking or
a performance advantage over the earlier layouts.

## Decision and limits

Proceed with an unstructured `work://shared/` and the short search-and-reuse
guidance. There is no evidence here that required `sessions/`, `topics/` or a
fixed notes filename is needed. Agents may introduce organization when useful.

This is sufficient evidence for the initial workflow, not a guarantee about
large, long-lived shared directories. Cases were isolated and small; simultaneous
writers, accumulated conflicting notes, remote storage, cleanup and cross-project
access were not tested. Future memory/journal integration must discover relevant
notes without assuming a reserved `notes.md` filename.

Production behavior is unchanged. This remains a throwaway prototype on
`memory-scope-evaluation`.

## Evidence and validation

The local raw-result paths below were not retained in this checkout; they are
historical locations, not downloadable repository artifacts.

- [Runner](run.el), [measurement/review renderer](summarize.py).
- Raw local results: Sol (`.scratch/memory-scope-freeform/sol.json`),
  Luna (`.scratch/memory-scope-freeform/luna.json`),
  Flash (`.scratch/memory-scope-freeform/flash.json`).
- Expanded review sheet (`.scratch/memory-scope-freeform/review.md`),
  including the final files after each reader.

Model settings and dependencies match the previous runs: Codex effort `none`,
Flash default, Emacs 31.1, gptel 20260906.334.
The adapter smoke test passed, including work-address isolation and absence of
prescribed folders in the prompt. All 192 package source files compiled without
warnings. Provider snapshots were deleted; evidence contains synthetic data only.
