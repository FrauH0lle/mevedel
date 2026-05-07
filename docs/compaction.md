# Conversation Compaction

Compaction reduces model-visible history while keeping the persisted
conversation recoverable. The implementation lives in
`mevedel-compact.el`; persisted segment rotation is handled by
`mevedel-session-persistence.el`.

## User model

`mevedel-compact` manually compacts the current chat. Automatic
compaction is enabled by default for persisted sessions and runs before
a request is sent when the estimated context crosses the configured
threshold.

During auto-compaction the view reuses the normal bottom spinner and
changes its status to `Compacting...`. When compaction completes, the
original request continues and the spinner returns to `Thinking...`.
The slash/manual command remains useful when the user wants to compact
with custom instructions.

The first-compaction accuracy notice is controlled by
`mevedel-compact-warn-on-completion`, enabled by default. It is emitted
as a plain `message`, not a `display-warning`.

## Trigger predicate

The effective context window comes from:

1. `mevedel-compact-context-limit` when non-nil.
2. The active `gptel-model` `:context-window` property, converted from
   thousands of tokens to raw tokens.
3. A 200000 token fallback.

Usable context is:

```
reserve = min(max(mevedel-compact-reserve-tokens,
                  effective max output tokens),
              context-window / 2)
usable = context-window - reserve
```

The reserve cap keeps small-context models from collapsing the default
fractional threshold to a near-zero value.

`mevedel-compact-token-threshold` accepts both styles:

- integer: absolute token threshold.
- float: ratio of usable context, default `0.80`.

`mevedel--compact-should-compact-p` also checks eligibility:

- `mevedel-compact-auto` must be non-nil.
- auto-compaction must not be disabled by repeated failures.
- no compaction request may already be in flight.
- the session must be writable, persisted, and on the active segment.

If the threshold is crossed but the session is not eligible, mevedel
warns once and lets the request proceed normally.

## Token estimate

Before a request is sent, mevedel still needs a local estimate. It uses
the historic chars/4 scan when no API baseline is available, ignoring
regions marked `gptel 'ignore` and excluding file-local variables.

After normal non-compaction requests, API-reported token usage from
gptel is recorded as `mevedel--known-token-baseline`. Future estimates
start from that measured baseline and add chars/4 only for text added
after the recorded marker. Compaction requests are explicitly excluded
from this baseline so the summarization call never pollutes chat usage
estimates.

## Request flow

Auto-compaction is installed as a gptel prompt transform. It runs after
skill/model overrides and before mention/reminder expansion. This order
matters:

- compaction uses the effective model/backend for threshold decisions.
- source segment rotation uses the original pending user text.
- the temporary request buffer is rebuilt after compaction and receives
  the transformed pending text, including reminders and mentions, for
  the actual request.

Compaction requests disable tools (`gptel-use-tools` and `gptel-tools`)
and use a no-tools prompt preamble. Failures retry up to three attempts
with exponential backoff. After repeated failures,
`mevedel--compact-auto-disabled` prevents further automatic attempts in
that buffer.

If automatic compaction finds that there is no old body to summarize
because the threshold is reached entirely by the preserved tail, it
returns `:skip` and sends the original request.

## Summary prompt

The summary prompt lives in `prompts/compaction/summary.md` and is
rendered by `mevedel--compact-prompt` with request-specific template
values. The generated summary is an anchored Markdown document with
fixed sections:

- Goal
- Constraints & Preferences
- Progress / Done / In Progress / Blocked
- Key Decisions
- Next Steps
- Critical Context
- Relevant Files
- Skills Invoked

On first compaction the prompt asks the model to create a new anchored
summary. On later compactions it provides the previous leading summary
and asks the model to update it. The update prompt treats the previous
summary as authoritative retained context: still-true details must be
kept, stale or contradicted details removed, and new facts merged in.
This is important because older segment contents are no longer present
in the model-visible prompt except through the previous summary.

Skill invocation records from the session are appended to the prompt so
summaries can preserve slash/model-side skill usage.

## Tail preservation

Compaction summarizes only the old body and preserves a recent tail
verbatim. The tail starts at the newest complete turn boundary that fits
both constraints:

- target turn count: `mevedel-compact-tail-turns`, default 2.
- budget: `mevedel-compact-tail-budget` of usable context, default
  0.25.

Tool output in both the preserved tail and summary request body is
truncated by character caps:

- `mevedel-compact-tail-tool-output-max`
- `mevedel-compact-body-tool-output-max`

The current unsent prompt is kept outside the summarized body. For
auto-compaction it is reattached after the new summary and preserved
tail, then the original request proceeds.

## Segment integration

Persisted sessions use split-on-compact:

1. Save and finalize the current `segment-NNNN.chat.org`.
2. Increment `mevedel-session-current-segment`.
3. Repoint the data buffer to the new segment file.
4. Rebuild the buffer with segment properties, a leading
   `#+begin_summary` block, preserved tail, and pending prompt.
5. Save the new segment and update the sidecar.

Old segment files remain on disk and stay available through
`mevedel-rewind`. The live view skips the leading summary block when
rendering the visible transcript, but the summary remains model-visible
for future requests.

If persistence is disabled, manual compaction falls back to the legacy
in-buffer mode: older content is marked `gptel 'ignore` and dimmed,
with a folded summary block inserted as an anchor.

## Defcustoms

All are in `mevedel-compact.el`:

- `mevedel-compact-auto` (default `t`)
- `mevedel-compact-context-limit` (default `nil`, use gptel model)
- `mevedel-compact-token-threshold` (default `0.80`)
- `mevedel-compact-reserve-tokens` (default `20000`)
- `mevedel-compact-tail-turns` (default `2`)
- `mevedel-compact-tail-budget` (default `0.25`)
- `mevedel-compact-tail-tool-output-max` (default `4000`)
- `mevedel-compact-body-tool-output-max` (default `8000`)
- `mevedel-compact-warn-on-completion` (default `t`)
