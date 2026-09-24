# Journal reasoning experiment, 2026-09-24

Registered before paid calls. Question: does reasoning improve selection and faithful retention of reusable knowledge, and does the current 4,000-token output limit impede it?

Six synthetic fixtures derived from mevedel workflows, with expected and forbidden facts in fixtures.json. Only `source` is sent to the model. Two intentionally empty cases, three short positive cases, and one long noisy/injection case. No private user transcript. Current production digest prompt unchanged. Fixtures test current reusable-knowledge policy, not general task summarization.

Six arms in arms.json; two replicates per fixture per arm (72 planned requests). DeepSeek Flash alias is V4.1 Flash; configured concrete DeepSeek backend supports disabled/high/max. Astra supports low through max, no none; configured Codex OAuth backend does not send server output limits. A missing client cap means provider default, NOT unlimited generation. DeepSeek disabled/high each tested with 4,000 and absent max_tokens. Astra low/high both have no explicit cap. No universal cross-provider reasoning scale is assumed.

Native generator and configured concrete transports; frozen copies of configured gptel libraries, manifest in dependencies.json. Eask isolated HOME. Native serialized request controls must pass dry-run before inference. Experimental overrides replace only digest policy's forced effort/cap and its final cap enforcement; production input wrapper, prompt, validation, 16 KiB visible-output guard and 120-second deadline stay in force. No changes in live Emacs. Private mode-0600 provider exports outside repository, deleted after workers finish. Credential/header data never included in results. No retries or selective replacement of failed results.

Four worker processes execute deterministically shuffled jobs (seed 940024). Timing includes request processing; concurrency/cache/provider load confound exact latency comparisons. Record native usage including reasoning tokens when reported, actual controls, accepted digest, status and elapsed time. Native usage may be absent for aborted requests; never infer zero cost from missing usage.

Assess validity, correct empty/publish decision, expected fact coverage, excluded facts, attribution, scope and correction handling. Semantic assessment by the investigating assistant, using anonymized output IDs before joining arm labels; this is not an independent human assessment. Repeated outputs are inspected once and inherit the same assessment. No model judge. All errors retained. Report per-case differences rather than treating correlated two-run repeats as a large independent sample. No-note cases with any retained excluded fact fail selection. Positive cases must preserve all decisive expected facts without a material false claim. Minor verbosity/duplication recorded separately from major selection/fidelity failure.

These fixtures cannot establish universal quality or a default reasoning setting for every backend. Cap-induced failure, endpoint mismatch and semantic failure are separate outcomes. Recommend the smallest policy supported by results. If no clear quality advantage is demonstrated, do not claim reasoning is unnecessary on all inputs.

## Recorder repair before measured run

The first four calls completed inference but the recorder tried to JSON-encode a native backend object and failed. No usable response or usage record was recovered. These are retained as four pilot measurement errors in pilot-measurement-errors.json, additional to the 72 planned measured calls. The recorder now uses an explicit scalar/result-field allowlist. No model output was inspected to choose a rerun. Private diagnostic logs from the failed recorder were removed.
