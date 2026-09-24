# Memory review output budgets — 2026-09-15

Source/task: main agent investigation of `Review output limit exceeded`, followed
by the user's approval to increase the cumulative budget and add diagnostics.

## Current: second authorized iteration and successful retry

User approved 64,000 cumulative tokens / 256 KiB accounted bytes / 64 calls,
then requested model-visible limits and system reminders. Implemented and
hot-loaded review, investigation, pass, and telemetry workspace source. The
32 KiB final reply, 64 KiB aggregate tool results, and 180-second deadline remain.
Initial prompt renders limits; at most two WAIT-time reminders fire at 75%/90%
pressure, coalescing jumps. Native parsing uses a simple string list and disables
message caching for reminders (Bedrock text shape / Anthropic cache-breakpoint
regressions). Completed-round accounting excludes nil transport failures.

Verification: 109 focused Eask tests passed in
`artifact://executions/execution-WY7ygG.log`; fixed one docstring warning, then
195 files compiled without warnings in `artifact://executions/execution-fWbagK.log`.
Scoped diff check passed. Bytecode cleanup completed. Static re-review PASS.

Main's single measured retry, pass
`1136c845fe1ed6ea9c6c348a8f6327d6efdcccd62b46721ce277280da5f19e8b`,
succeeded in 98.160 seconds, covering six digests and publishing three pending
proposals, none applied; zero eligible digests remained. Terminal telemetry at
`.mevedel/diagnostics/telemetry-log.el:22` matches the callback: 14,919 provider
output tokens, 15,374 estimated tokens, 60,599 accounted bytes = 51,600 reasoning
+ 8,260 replies + 739 tool-call bytes; 7 admitted calls, 4 completed rounds,
8,099 final-reply bytes, no exhausted guard. Request buffer and timer retired,
running registry empty. Proposal statuses independently read as pending.

Native preflight used the same DeepSeek view caller described below, with
streaming enabled, max_tokens 64000, no thinking/effort field, six candidates,
12,057 estimated input tokens. Live source fingerprints below were rechecked
unchanged; Eask dependencies still differ. This one success does not establish
causality for prompt wording or general provider reliability.

## Historical: first implemented iteration

- `mevedel-memory-review.el`: frozen configurable defaults of 16,000 cumulative
  output tokens and 65,536 accounted bytes. Final proposal parser remains 32 KiB.
  Native supported output limits shrink with remaining cumulative budget.
- Review snapshots, pass results, and terminal telemetry preserve output bytes,
  estimated tokens, current-round reply bytes, and the failing output guard's
  category/threshold. No generated text is logged.
- 97 focused Eask tests passed; all 195 package files compiled without warnings.
  Used cached Eask CLI via node because npx registry access was unavailable.
  Log: `artifact://executions/execution-LBzEp1.log` (owned by the originating session).
- Loaded review, pass, and telemetry source files explicitly into live Emacs;
  `symbol-file` confirmed workspace `.el` definitions, not Straight bytecode.
- Scoped diff whitespace check passed. Unrelated existing whitespace failures
  in `docs/backlog.md:24,30` were preserved.

## Controlled native retry — observation, not end-to-end success

Used the existing root view caller, whose memory workload is DeepSeek V4 Flash
on concrete `gptel-deepseek`. The root data buffer instead resolves Codex, so
using it would not reproduce the failed caller. No provider dependency was
substituted or reloaded: dry-run and paid retry used the same live Emacs runtime.
Native dry-run showed streaming=true, max_tokens=16000, no explicit thinking or
reasoning_effort fields, Read/Glob/Grep tools, five candidate digests, and 10,624
estimated input tokens. No paid call occurred during preflight.

One retry in propose mode, pass
`9b8736ce72d66b2240cd9679b3cee16cbec3ccf017658e25a3ecb74c423ec034`, ended after
101.712 seconds with **Tool call budget exhausted** (the separate 20-call cap).
Terminal telemetry at `.mevedel/diagnostics/telemetry-log.el:15` recorded:
13,248 provider output tokens; 53,745 accounted bytes; 13,638 estimated tokens;
zero current-round reply bytes; nil output budget-kind/threshold. No proposals
or coverage published; all five digests remain unreviewed. Do not report this
as a successful consolidation or increase the separate tool cap without scope.

## Accounting limitation retained

Tool output charges use normalized executable names/parsed arguments at TOOL,
not raw JSON spelling or in-flight argument fragments. They are not raw wire or
billing bounds. This predates the change and is now explicit in docs/memory.md.
Reviewer also found, and regressions now cover, loss of available provider usage
on non-streaming reasoning overflow and dispatch after exactly exhausted bytes.

## Native dependency fingerprints

Loaded libraries were under `/home/roland/.emacs.d/straight/build/`:

| Library | Loaded .elc SHA-256 | Backing source SHA-256 |
|---|---|---|
| gptel/gptel-request | 5dfbe43679807e03529cf7fa40231981195ea727d343672842ce0d82ba9dcf05 | 9e586576401ab3c5d4cb37323914bf015f0ea2653ccf43ecdb48fa27f9723c9f |
| gptel/gptel-openai | c09dcabb42de3da457b8142bfa2004e5258f32b807c8df58c532cc454e170b2d | 010e4088589f3b704446477579bb929da3ae2313676e8eb172cfab561359c639 |
| gptel/gptel-openai-extras | 96f763e704e0d97b9e0a99814599ab9212be9a16b3e2e75e6a20eb44eb29db0b | 72fff168a8dfb04fa10eed6988c34f88f1dbd65bc0e2347f97c77cc308dcc9ad |
| gptel-agent/gptel-agent | f00f9afc707e8f76ae1d941da4a0362b991b07ed3873ce45e1dd49c7ac7aa20c | 320bfe77648cbf90c6374cd269e92f2fdf5e60166b0d6671ce62f8c567c7809b |

gptel backing sources are in `/home/roland/gptel/`; gptel-agent backing source is
`/home/roland/.emacs.d/straight/repos/gptel-agent/gptel-agent.el`. Eask dependencies
have different source hashes; local tests are not a dependency-equivalent live
provider experiment. Upstream refresh failed on sandbox directory grants.
