---
name: schedule
description: Schedule a wake-up for this session or a workspace schedule with the Schedule tool
context: inline
user-invocable: true
argument-hint: "[workspace] [timing] prompt"
---

Use the `Schedule` tool to act on `$ARGUMENTS`, read as
`[target] [timing] prompt`. Add nothing the tool does not do.

- **Target.** A leading `workspace` means `target: "workspace"`; otherwise
  create a wake-up for this session. A workspace schedule also needs a short
  `name`, and its `note` must stand alone for a fresh session that knows
  nothing of this conversation. The user approves it on a card, so say that a
  proposal is pending instead of claiming it exists.
- **Timing** becomes `when`:
  - one-shot: a duration (`20m`, `1h30m`) or a local time
    (`2026-11-21 10:00`);
  - interval: `every 20m` (at least a minute);
  - cron, local time: `0 9 * * 1-5`.
- **Prompt** becomes `note`: what to check and when the user should hear.
  Woken turns and schedule runs finish quietly, so tell it to call `Notify`
  when the user should hear.
- If the timing is missing or ambiguous, ask once; do not guess a time.
- Report the id and next run from the result.

Examples:

- `$schedule 20m check CI and tell me only if it failed` is a one-shot wake-up.
- `$schedule every 30m babysit PR 412` repeats.
- `$schedule workspace 0 7 * * 1-5 triage new issues` proposes a workspace
  schedule.

## Without a fixed interval

For "keep an eye on X until Y", schedule one one-shot wake-up whose note
restates the goal and the stop condition. When it arrives, check, then either
schedule the next one-shot with a delay suited to what you saw, or stop and
call `Notify` if the user should hear. Do not schedule the next wake-up once
the goal is met.
