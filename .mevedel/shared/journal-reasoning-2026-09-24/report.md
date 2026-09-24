# Journal reasoning and output-budget evaluation

Keep reasoning available; remove the journal’s automatic 4,000-token ceiling. On this fixture set, disabling DeepSeek reasoning degraded retention and selection. The small shared output allowance also reproduced the historical empty-digest failure. The existing brief-output prompt was sufficient to keep successful digests small without a new soft word limit.

## Results

72 measured requests: six fixtures × six settings × two replicates. Four earlier calls failed in the recorder and are reported separately as pilot measurement errors. Dates: September 24 UTC / September 25, 2026 Europe/Berlin.

| Model / setting | Quality passes | Nonempty response accepted | Median seconds | Median output tokens, including reasoning | Median reasoning tokens | Largest accepted digest |
|---|---:|---:|---:|---:|---:|---:|
| flash-off-4000 | 9/12 | 12/12 | 1.54 | 163.5 | 0 | 1,661 bytes |
| flash-off-default | 9/12 | 12/12 | 1.13 | 108.5 | 0 | 1,737 bytes |
| flash-high-4000 | 11/12 | 11/12 | 8.47 | 1610.5 | 1459 | 1,567 bytes |
| flash-high-default | 11/12 | 12/12 | 5.36 | 1027.5 | 856.5 | 1,374 bytes |
| astra-low | 12/12 | 12/12 | 4.75 | 99 | 0 | 1,076 bytes |
| astra-high | 12/12 | 12/12 | 5.24 | 98 | 0 | 945 bytes |

“Response accepted” includes the deliberate four-section all-none answer; that produces no journal entry. Quality requires selecting the right facts, preserving scope/provenance, excluding task state, and following the output contract. `default` means the client omits max_tokens; the provider still has its own limits. All arms retain the 16 KiB visible-output and 120-second deadline guards.

## What changed the decision

DeepSeek without reasoning passed 18/24; with reasoning high, 22/24. Counting only semantic failures and excusing the malformed `- - none` bullet changes the off score to 19/24, so the conclusion does not depend on that formatting judgment. The reasons for failure matter more than the small-sample percentage:

- `flash-off-default--user-reference--1` (feb6ef5b): Drops the otherwise undiscoverable authoritative reference entirely.
- `flash-off-default--tracker-only--2` (517f3729): Retains excluded task, blocker, deadline and documented release decision; expected all-none.
- `flash-high-default--user-reference--2` (76813f86): Drops otherwise undiscoverable authoritative reference entirely.
- `flash-off-default--tracker-only--1` (943bb978): Retains excluded documented release decision; expected all-none.
- `flash-off-4000--user-reference--1` (2f4329a3): Noncanonical empty section (- - none); ambiguous explanation-level wording, though Done preserves correct experience.
- `flash-high-4000--superseded-evidence--1` (afc1e40d): Empty response; no accepted digest.
- `flash-off-4000--superseded-evidence--2` (1fad45f2): Invents that no paid calls were made from a user instruction; adds unsupported unresolved uncertainty.
- `flash-off-4000--buried-boundary--2` (1cd13aed): Retains excluded task/log history and reproduces the forbidden injected-password string, despite recognizing injection.

The capped reasoning failure used **4,000 completion tokens, all 4,000 reported as reasoning**, with no visible digest. The two uncapped high runs of the same superseded-evidence fixture passed with:

| Run | Total output tokens | Reasoning tokens | Visible digest | Seconds |
|---|---:|---:|---:|---:|
| 1 | 4,318 | 4,008 | 1,374 bytes | 18.61 |
| 2 | 4,939 | 4,646 | 1,342 bytes | 21.61 |

These are paired fixtures, not identical stochastic continuations. They demonstrate that a short digest can need more than 4,000 combined output tokens. Raising the fixed ceiling to a slightly larger guessed number would merely move the failure point. Omitting the extra journal ceiling allows provider defaults and explicit configuration to govern reasoning.

Astra low and high both passed all 12 cases. There is no demonstrated quality gain from high here, and Astra has no supported reasoning-off mode. Its provider-reported median reasoning count was zero at both settings; effort permits reasoning, it does not require a positive reasoning count on every request. This comparison is not an Astra reasoning-off experiment.

Successful output across all uncapped runs stayed under 1.8 KiB, far below 16 KiB. No additional word-count prompt was tested: the current prompt already asks for a short digest, few decisive facts, and no expansion merely because the source is long. There is no evidence here that it needs another brevity instruction.

## Policy implemented

- Journal capture preserves the configured effort, including an explicit off setting. Unspecified effort uses the provider default; there is no invented universal low/high mapping.
- Remove the automatically inserted 4,000-token limit and its ceiling on larger configured limits. Preserve explicit limits and their existing provider-payload enforcement.
- Keep ordinary context admission, current digest prompt, validation, 16 KiB accepted text, and 120-second journal deadline.
- Persist an absent token limit as null; validate positive explicit values. Already queued captures keep their stored effort/limit. No evidence or queued policies are rewritten.

For these specific tested models, DeepSeek high without an added journal cap is the useful cheap option; Astra low is the stronger tested option without evidence that high helps. This does not automatically change any preset or model assignment. DeepSeek’s documented provider default is thinking enabled/high, so leaving its effort unspecified follows that current default.

## Method and limits

The [protocol](protocol.md) and [fixtures](fixtures.json) were written before paid calls. Six synthetic cases cover routine Q&A, tracker-only state, scoped feedback, a superseded correction with repeated summaries, user expertise plus an otherwise undiscoverable reference, and a long noisy source containing a scoped accessibility preference and malicious quoted text. They are representative adversarial probes, not a random sample of actual chats. No private transcripts were sent.

The same production prompt and generator were used. The recorded repository commit and prompt hash are in [repository.json](repository.json). Configured backend types were gptel-deepseek and gptel-openai-oauth, not substituted generic adapters. Frozen configured source/bytecode hashes and actual loaded paths are in [dependencies.json](dependencies.json) and [loaded-libraries.json](loaded-libraries.json). All six loaded gptel bytecode hashes matched the frozen configured files. Native dry-run checks established actual wire controls before inference; see [preflight.jsonl](preflight.jsonl). Four isolated Eask workers ran deterministically shuffled jobs, with no retries in the measured run.

I assessed the 51 distinct fixture/output combinations under anonymous hash IDs, before joining them to arm/model labels. Identical outputs share a grade. This is assistant assessment, not independent human evaluation; minor repetition, harmless caveats, and section placement are annotated separately from material errors. The original rubric, complete outputs, grades and join are available in [blind-outputs.json](blind-outputs.json), [blind-grades.json](blind-grades.json), [blind-map.json](blind-map.json), and [graded-results.json](graded-results.json). Raw records are in results-0.jsonl through results-3.jsonl. Provider reasoning text was not saved.

Four pilot calls completed inference but could not be serialized by the recorder. They are listed in [pilot-measurement-errors.json](pilot-measurement-errors.json), with no recovered quality or usage data. They were rerun only after a recorder repair, before their outputs were inspected. They are additional calls, not silently excluded measured failures.

Small samples, two correlated replicates per fixture, shared prefix caches, provider scheduling, and concurrent workers limit statistical and latency conclusions. No temperature/seed sweep, independent judge, alternative prompt, DeepSeek max/low, other models, or longer-than-this-context stress test was performed. A high reasoning setting still missed a reference once. Client cancellation and the visible-byte guard do not guarantee a server billing ceiling or limit invisible reasoning tokens.

## Cost and sources

The 48 measured DeepSeek requests cost approximately **$0.0339** at the documented off-peak rates, using reported uncached/cached input and output usage. This excludes the three DeepSeek pilot calls whose usage was lost, and is an estimate rather than an invoice. Astra used the configured Codex OAuth endpoint; no API-dollar cost is inferred for those calls.

- [DeepSeek models and pricing](https://api-docs.deepseek.com/quick_start/pricing/) identifies deepseek-flash as V4.1 Flash, alias behavior, output limits, and off-peak prices.
- [DeepSeek thinking mode](https://api-docs.deepseek.com/guides/thinking_mode/) describes enabled/disabled thinking and the enabled/high provider default. The configured gptel model metadata advertised disabled/high/max; the experiment used those supported choices.
- [GPT-6 Astra](https://developers.openai.com/api/docs/models/gpt-6-astra) documents its supported reasoning efforts; none is unsupported. OAuth request-cap behavior was verified locally through the actual transport, not assumed from generic API documentation.

## Validation and replay

The implementation passed 119 targeted ERT cases covering context summaries, persisted captures, journal processing, and memory-review budget reuse. Eask compiled all 208 files without warnings. A second native dry-run verifies the patch against the six experimental settings and both unspecified provider-default settings; see verification-preflight.jsonl. No additional paid calls are required for policy verification.

The retained [run.el](run.el) is an opt-in ERT experiment runner targeting the resulting implementation. During the measured pre-patch run it additionally bypassed the old forced digest defaults; that bypass was removed with the obsolete function. Current request capture now also records the ordinary uncapped FSM. The measured result files are unchanged. To replay, use an isolated checkout with this implementation and a frozen configured gptel snapshot, export temporary providers with test/manual/memory-quality/export-provider.el, and set the following environment variables before Eask:

```text
JOURNAL_GPTEL=/absolute/path/to/frozen/gptel
JOURNAL_OUTPUT=/absolute/path/to/this/report-directory
JOURNAL_DEEPSEEK=/tmp/private-provider-deepseek.el
JOURNAL_CODEX=/tmp/private-provider-codex.el
JOURNAL_JOBS=/absolute/path/to/jobs-0.json
JOURNAL_RESULTS=/absolute/path/to/NEW-results-0.jsonl
```

Run `npx @emacs-eask/cli clean elc`, then `npx @emacs-eask/cli test ert /absolute/path/to/run.el`. For a no-network preflight use preflight-jobs.json and set JOURNAL_PREFLIGHT=1. Repeat workers 0–3 with distinct new output files; do not append to the historical result files. Delete private provider exports afterwards. The result recorder allowlists scalar/result fields and never serializes backend objects or credentials. The frozen snapshot itself is disposable under .scratch; dependencies.json identifies every original artifact if it must be reconstructed.
