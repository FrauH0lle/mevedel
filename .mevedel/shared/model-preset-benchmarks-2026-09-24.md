# Benchmark evidence for model presets — 2026-09-24

Research snapshot for a proposal, not implemented configuration. Measurements below belong to their original harnesses; none measures mevedel.

## Artificial Analysis

Retrieved model pages and their embedded JSON-LD datasets. Intelligence Index version on the pages is v4.3.2. Cost/task is a weighted benchmark aggregate including input, cache read/write, reasoning and answer tokens. Decode minutes/task excludes time to first token and tool/other overhead; it is not end-to-end coding-task latency. [Definitions](https://artificialanalysis.ai/#intelligence).

| Model / effort | AA Intelligence | USD/task | Decode min/task |
|---|---:|---:|---:|
| [Astra low](https://artificialanalysis.ai/models/gpt-6-astra-low) | 45.78 | 0.818 | 1.63 |
| [Astra medium](https://artificialanalysis.ai/models/gpt-6-astra-medium) | 49.57 | 1.541 | 3.53 |
| [Astra high](https://artificialanalysis.ai/models/gpt-6-astra-high) | 50.92 | 1.725 | 3.94 |
| [Astra xhigh](https://artificialanalysis.ai/models/gpt-6-astra-xhigh) | 52.39 | 2.309 | 5.36 |
| [Astra max](https://artificialanalysis.ai/models/gpt-6-astra) | 52.67 | 3.258 | 8.87 |
| [Sol low](https://artificialanalysis.ai/models/gpt-6-sol-low) | 33.90 | 0.132 | 0.61 |
| [Sol medium](https://artificialanalysis.ai/models/gpt-6-sol-medium) | 39.78 | 0.248 | 1.00 |
| [Sol high](https://artificialanalysis.ai/models/gpt-6-sol-high) | 42.82 | 0.375 | 1.76 |
| [Sol xhigh](https://artificialanalysis.ai/models/gpt-6-sol-xhigh) | 44.10 | 0.532 | 2.72 |
| [Sol max](https://artificialanalysis.ai/models/gpt-6-sol) | 47.53 | 1.056 | 4.85 |
| [Luna low](https://artificialanalysis.ai/models/gpt-6-luna-low) | 20.92 | 0.00448 | 0.28 |
| [Luna medium](https://artificialanalysis.ai/models/gpt-6-luna-medium) | 29.46 | 0.0173 | 1.33 |
| [Luna high](https://artificialanalysis.ai/models/gpt-6-luna-high) | 32.15 | 0.0286 | 2.55 |
| [Luna xhigh](https://artificialanalysis.ai/models/gpt-6-luna-xhigh) | 33.88 | 0.0417 | 3.52 |
| [Luna max](https://artificialanalysis.ai/models/gpt-6-luna) | 37.26 | 0.0681 | 5.89 |
| [DeepSeek V4.1 Flash max](https://artificialanalysis.ai/models/deepseek-v4-1-flash) | 39.46 | 0.265 | 4.71 |
| [DeepSeek V4 Pro 0813 max](https://artificialanalysis.ai/models/deepseek-v4-pro) | 36.00 | 0.674 | 9.31 |

| Model | Input / cached input / output USD per million tokens | Output tokens/sec at max |
|---|---:|---:|
| [GPT-6 Astra](https://artificialanalysis.ai/models/gpt-6-astra) | 10 / 1 / 50 | 51.8 |
| [GPT-6 Sol](https://artificialanalysis.ai/models/gpt-6-sol) | 2 / 0.20 / 10 | 104.4 |
| [GPT-6 Luna](https://artificialanalysis.ai/models/gpt-6-luna) | 0.10 / 0.01 / 0.50 | 132.2 |
| [DeepSeek V4.1 Flash](https://artificialanalysis.ai/models/deepseek-v4-1-flash) | 0.30 / 0.006 / 1.20 | 231.8 |
| [DeepSeek V4 Pro 0813](https://artificialanalysis.ai/models/deepseek-v4-pro) | 1.32 / 0.044 / 3.96 | 68.8 |

OpenAI cache writes carry a 25% premium according to [AA's Sol/Luna release analysis](https://artificialanalysis.ai/articles/gpt-6-sol-and-luna-push-the-cost-efficiency-frontier). That analysis also reports GPT-6 Sol max scoring 57 on its Coding Agent Index at $2.99/task, and Luna max scoring 41; Luna's DeepSWE v1.1 result is 64% in Codex. These are **AA's Codex-harness measurements**, not the Datacurve leaderboard's mini-swe-agent measurements.

## DeepSWE v1.1

The [live leaderboard](https://deepswe.datacurve.ai/) shows 113 tasks, updated September 22, 2026. Its [public JSON artifact](https://deepswe.datacurve.ai/artifacts/v1.1/leaderboard-live.json) exposes all effort configurations. GPT-6 Sol and GPT-6 Luna are absent from both the page and full artifact. Rows named GPT-5.6 Sol/Luna must not be treated as GPT-6 evidence.

| Model / effort | Pass@1 | Displayed CI half-width | Mean USD/task | Mean duration, minutes | Mean steps |
|---|---:|---:|---:|---:|---:|
| Astra low | 67.0% | 1.3 pp | 1.60 | 10.22 | 19.5 |
| Astra medium | 72.8% | 2.6 pp | 3.08 | 14.73 | 26.0 |
| Astra high | 73.2% | 3.4 pp | 3.92 | 17.31 | 27.4 |
| Astra xhigh | 74.1% | 2.9 pp | 4.43 | 18.87 | 28.8 |
| Astra max | 73.2% | 0.8 pp | 7.50 | 33.05 | 28.5 |
| DeepSeek V4 Pro max | 62.8% | 6.3 pp | 1.67* | 36.88 | 154.7 |
| DeepSeek V4 Flash max | 53.3% | 3.6 pp | 0.46* | 23.98 | 152.9 |

Table sources: [live page](https://deepswe.datacurve.ai/) and [full artifact](https://deepswe.datacurve.ai/artifacts/v1.1/leaderboard-live.json). Each configuration ran four whole-benchmark attempts (452 trials). CI is run-to-run standard error multiplied by 1.96, not uncertainty across a random sample of all possible programming tasks. Context failures and timeouts count as failures; infrastructure errors are excluded.

*The live page and artifact disagree on DeepSeek cost: artifact means are $0.241 Pro and $0.100 Flash, while live display shows $1.67 and $0.46. This note uses displayed live cost, without claiming the cause is established. Astra values agree. DeepSWE's old V4 Flash row is **not** evidence for V4.1 Flash. Likewise the displayed generic V4 Pro label does not establish the exact API snapshot used.

The benchmark uses mini-swe-agent's bash-only harness, so its ranking need not transfer intact to mevedel. Tasks cover TypeScript, Go, Python, JavaScript and Rust; Emacs Lisp is absent. Bug localization and refactoring are underrepresented. [Methodology and limitations](https://deepswe.datacurve.ai/blog/deepswe#limitations).

## DeepSeek API mapping

Current official documentation maps `deepseek-flash` to **DeepSeek-V4.1-Flash** and `deepseek-v4-pro` to **DeepSeek-V4-Pro-0813**. Legacy `deepseek-v4-flash` requests now serve V4.1 Flash: that API name does not reproduce the older benchmark model. Flash supports images and both models support tool calls, Responses and Anthropic API formats. The price table above uses peak rates; official off-peak rates are half. [DeepSeek models and pricing](https://api-docs.deepseek.com/quick_start/pricing).

Thinking is enabled by default at high effort. Chat Completions uses `thinking: {type: "enabled"}` and `reasoning_effort: "max"`; Responses uses `reasoning: {effort: "max"}`. Actual effort levels are low/high/max; medium and xhigh map to high, and ultra maps to max. [DeepSeek thinking guide](https://api-docs.deepseek.com/guides/thinking_mode).

## Interpretation for a proposal

These are judgments from the evidence, not benchmarked mevedel assignments:

- Astra medium is a defensible quality-oriented root: on DeepSWE it is close to high/xhigh with overlapping reported intervals, at lower cost and duration. Max nearly doubles xhigh's cost with no measured coding gain.
- Sol high is a defensible worker baseline: notably stronger than medium in AA at modest additional cost; xhigh/max remain explicit escalation choices. Direct Datacurve GPT-6 Sol evidence is unavailable.
- Luna medium suits bounded exploration and lightweight work. Low loses considerably on AA; max remains cheap in dollars but consumes more decode time than Sol max on AA's task mix.
- V4.1 Flash is the better-supported current DeepSeek candidate than Pro 0813 for a general secondary role: higher AA score, lower cost and higher generation speed. There is no same-version Datacurve coding result to justify replacing Sol for implementation.
- A DeepSeek verifier offers a different model family, but these sources do not measure review quality or prove independent error modes. Treat any review-diversity benefit as a hypothesis to evaluate in mevedel, and retain Astra/Sol for consequential decisions.
