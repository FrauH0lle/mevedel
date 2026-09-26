# Proposed mevedel model presets — 2026-09-24

Installed in `~/.emacs.d/site-lisp/config.el`, with journal assignments revised
on 2026-09-25 after commit `0dc15de`. The examples below use placeholder backend
names; the installed configuration uses Codex OAuth and DeepSeek. The aim
is reliable day-to-day engineering with cheaper delegated and background work.
The workload assignments are engineering judgments, not measured mevedel results.

## Recommendation

| Workload | GPT-6 only | GPT-6 + DeepSeek |
| --- | --- | --- |
| Main conversation | Astra / medium | Astra / medium |
| Planning, reviewer | Astra / high | Astra / high |
| Accepted-plan implementation | Sol / high | Sol / high |
| Worker | Sol / high | Sol / high |
| Verifier | Sol / high | DeepSeek V4.1 Flash / max |
| Explorer | Luna / medium | Luna / medium |
| Buddy, memory consolidation, compaction/handoff summaries | Sol / medium | Sol / medium |
| Journal digest | Astra / low | DeepSeek V4.1 Flash / high |
| Session naming | Luna / none | Luna / none |
| Guardian, if automatic permission review is enabled | Sol / low | Sol / low |

Use the GPT-6-only preset as the default recommendation. The hybrid introduces
a different model family for adversarial verification, where the root can assess
its findings. Neither source proves that this diversity improves mevedel outcomes.
It is a candidate to evaluate, not a claim that DeepSeek outperforms Sol at coding.
The hybrid also uses DeepSeek for journal digests, based on the local evaluation
described below.
The presets do not enable automatic permission review or require agent delegation.

## Evidence and limits

[Artificial Analysis](https://artificialanalysis.ai/#intelligence) reports these
approximate Intelligence Index scores and average evaluation-task costs at the
specified effort: Astra medium 49.57 / $1.54, Astra high 50.92 / $1.73,
Sol high 42.82 / $0.375, Luna medium 29.46 / $0.0173, and DeepSeek V4.1
Flash max 39.46 / $0.265. These are broad evaluation results, not coding-only
scores or forecasts of a mevedel session's bill. Lower-effort housekeeping
assignments above are workload-specific judgments rather than results established
by these comparisons.

[DeepSWE v1.1](https://deepswe.datacurve.ai/) has 113 long-horizon coding tasks.
Astra medium achieves approximately 72.8% at $3.08 per task; high 73.2% at $3.92;
xhigh 74.1% at $4.43; max 73.2% at $7.50. The small score differences do not
establish a reliable ordering, but the measured cost increase argues against max
as a default. Medium is a reasonable starting point; use high or xhigh explicitly
for difficult work. These runs use mini-swe-agent rather than mevedel.

DeepSWE does not currently report GPT-6 Sol or Luna. Its GPT-5.6 entries are not
substitutes. Its older DeepSeek V4 Flash result is not evidence for V4.1 Flash.
Separately, [AA's Sol/Luna release analysis](https://artificialanalysis.ai/articles/gpt-6-sol-and-luna-push-the-cost-efficiency-frontier)
reports Coding Agent Index scores of 57 for Sol max and 41 for Luna max, using
Codex. It also reports Luna max at 64% on DeepSWE v1.1 in that harness. This
supports preferring Sol for implementation, but does not measure Sol high or
Luna medium in mevedel or fill the missing Datacurve leaderboard rows.
Artificial Analysis and DeepSWE costs come from different task distributions and
must not be combined into one score or cost ranking.

DeepSeek V4.1 Flash max scores above V4 Pro 0813 max on the broad AA index
(39.46 versus 36.00) at lower average evaluation cost ($0.265 versus $0.674),
which motivates choosing Flash for the hybrid. It is not a universal dominance
claim. AA's weighted decode time is about 4.71 minutes for Flash max versus 1.77
minutes for Sol high; lower evaluation cost does not imply lower latency.
See the accompanying [benchmark research](model-preset-benchmarks-2026-09-24.md)
for source pages, effort sweeps, version distinctions, and data caveats.

## Proposed configuration

Define these after `mevedel-install` has registered the built-in parent presets.
`OpenAI` and `DeepSeek` below are example registered gptel backend names; replace
them with the actual names, including the prefixes in provider strings. Keep the
concrete backend type appropriate to the user's authentication/transport.

```elisp
(mevedel-define-preset mevedel-gpt6
  :description "GPT-6: Astra lead, Sol workers, Luna exploration"
  :parents (mevedel-implement)
  :backend "OpenAI"
  :model 'gpt-6-astra
  :reasoning-effort 'medium
  :model-tiers
  ((fast :provider "OpenAI:gpt-6-luna" :effort medium)
   (balanced :provider "OpenAI:gpt-6-sol" :effort high)
   (strong :provider "OpenAI:gpt-6-astra" :effort high))
  :model-workloads
  ((planning :tier strong)
   (plan-implementation :tier balanced)
   (worker :tier balanced)
   (explorer :tier fast)
   (verifier :tier balanced)
   (reviewer :tier strong)
   (naming :tier fast :effort none)
   (guardian :tier balanced :effort low)
   (buddy :tier balanced :effort medium)
   (journal :tier strong :effort low)
   (memory :tier balanced :effort medium)
   (summarization :tier balanced :effort medium)))

(mevedel-define-preset mevedel-gpt6-deepseek
  :description "GPT-6 team with DeepSeek verification and journal"
  :parents (mevedel-gpt6)
  :model-workloads
  ((verifier :provider "DeepSeek:deepseek-flash" :effort max)
   (journal :provider "DeepSeek:deepseek-flash" :effort high)))
```

The hybrid replaces the verifier and journal workload entries. The current mevedel merge
replaces whole entries by workload name, so it does not retain the parent's
`:tier balanced` alongside the new `:provider`.

Optional adoption, once the backends/models are registered:

```elisp
(setf (alist-get 'implement mevedel-action-preset-alist) 'mevedel-gpt6)
;; Or use 'mevedel-gpt6-deepseek as the value above.
```

`mevedel-default-chat-preset` stays the action symbol `implement`; its value is
not the custom preset name. Existing selected sessions require their own preset
selection. Named worker roles use the worker mapping; a child launched without a
role inherits the delegator. Explicit Agent and skill overrides can supersede
these defaults, so this proposal is not an enforced provider allowlist.

The `plan-implementation` workload initializes the first approval's model and
effort for both Plan flows. `M` can override it; retained selections survive
revisions, preset changes, acceptance, and retries. This does not change the
model for ordinary coding turns. Both installed presets inherit Sol high here.

## Integration observations

- GPT-6 Sol and Luna are absent from the default model catalogs in both the
  refreshed upstream gptel checkout and the local `/home/roland/gptel` sources
  inspected today. They must be registered on the chosen backend before these
  presets can resolve them. The installed site-lisp configuration now registers
  both on the existing Codex backend. Official model metadata and supported
  efforts are available in the [OpenAI catalog](https://developers.openai.com/api/docs/models).
- [DeepSeek's current model table](https://api-docs.deepseek.com/quick_start/pricing)
  maps `deepseek-flash` to DeepSeek V4.1 Flash. The local gptel source supports
  `disabled`, `high`, and `max` and serializes its native thinking controls.
  Upstream and local gptel differ; no live provider request was made to validate
  this proposal.
- Commit `0dc15de` supersedes the original Sol/none journal recommendation.
  The [local evaluation](journal-reasoning-2026-09-24/report.md) found Astra low
  and high both passed 12/12 fixtures; DeepSeek high without an added token cap
  passed 11/12, versus 9/12 with reasoning off. Sol was not evaluated. Use Astra
  low for the GPT-6 journal and DeepSeek Flash high for the hybrid journal.
  These are small synthetic-fixture results, not a universal quality guarantee.
  The [current policy](../../docs/memory.md) preserves reasoning and adds no
  automatic token cap; explicit limits still apply where supported. Existing
  queued captures retain their frozen policies. The presets add no token limit.
- Session preset memory policy applies to caller-scoped consolidation. Automatic
  idle consolidation uses global policy rather than restoring a session preset;
  configure `mevedel-model-workloads` globally if the same selection is wanted
  there. See the [memory contract](../../docs/memory.md#model-selection).

The original presets were installed and loaded in the live Emacs process.
The subsequent `plan-implementation` addition passed 175 focused ERT cases,
461 related fixture-consumer cases, and an isolated personal-preset check;
208 files compiled without warnings. The personal check used current local
gptel sources (preferring them over stale checkout bytecode) and verified native
Codex payloads select Sol/high while the root stays Astra/medium, without paid
calls. This is not a live-Emacs transport check: no Emacs server was running at
handoff, so the latest definitions were not hot-loaded. They load on the next
startup. Reapply the preset in restored sessions to adopt the new workload;
retained approval selections and queued captures are not rewritten.
