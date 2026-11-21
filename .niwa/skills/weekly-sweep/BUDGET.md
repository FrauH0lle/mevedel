# Budget

The sweep spends the **target subscription** chosen at intake (Claude or
Codex); its weekly reset is the stop time. Its quota comes in **5-hour
windows**; the window, not the weekly limit, binds during a sweep. The
partner model draws on the other subscription and keeps working when a
target window is spent. The burn rates below were measured on the Claude
subscription; measure the Codex rates in the first windows of a Codex sweep
and record them here.

## Reading the quota

Read the quota with the `SubscriptionUsage` tool: plain readings for both
subscriptions. If it fails, the running Emacs predates a fix; restarting it is
the user's call, so ask before improvising an `Eval`.

Record each reading in the ledger as time, weekly %, window % and both reset
times. Read after each worker result, on each 10-minute reminder, and at least
hourly.

## Plan

**Start time.** The 5-hour window binds, not the weekly limit. On the Claude
subscription one full window costs 11 weekly points (measured 2026-11-07 in
three windows: 81 window points = 9 weekly points each). Windows end at about
88% in practice, so plan with 10 weekly points per window:

- windows needed = ceil(remaining weekly % / 10);
- start = weekly reset − 5 h × windows needed.

Example: 57% left needs 6 windows, so start 30 h before the reset. Starting
later leaves the rest of the week unspent (2026-11-07 started 18 h before the
reset with 57% left and ended at 75%). A window only starts with the next
request, so resume work at each window reset with a wake timer.

Write the schedule into the ledger before the first dispatch:

- **Windows left**: the 5-hour windows that start before the weekly reset.
- **Per-window allowance**: remaining weekly % divided by windows left. A full
  window costs roughly 10 weekly points, so the allowance usually exceeds
  what one window can spend: keep Claude workers busy and let the window be
  the limit.
- **Coordinator reserve**: about 40% of each window. Four Claude workers burn
  about 2 window points a minute; a long coordinator context about 0.5.
- **Pauses** (window spent, waiting for its reset): run the watchdog with
  `--heartbeat 50` and arm one wake timer for the reset. The main
  conversation's prompt cache lives 1 hour on the subscription; a heartbeat
  every 50 minutes reads the cached context (≈0.05× its tokens) instead of
  rebuilding it at resume (≈2×). On each `HEARTBEAT` line, end the turn at
  once with a one-word reply. Skip heartbeats for pauses under an hour.
- **Hard stop** at the weekly reset, or at the later stop time the user gave at
  intake:
  - stop − 60 min: dispatch cutoff; nothing new starts.
  - stop − 30 min: no new review round or full suite starts.
  - stop − 10 min: every worker commits a green checkpoint and writes
    `handoff-NAME.md` beside the ledger, then stops.
- **Multi-round chains** (security reviews, sandbox work) start in the first
  half of the day, so their last round finishes inside a window and before
  the cutoff.

## During the run

- Start a Monitor that prints a reminder every 10 minutes.
- Near 90% of a window, interrupt Claude workers cleanly between edits (a turn
  that hits the limit dies mid-edit) and dispatch partner-model work meanwhile. Resume
  the Claude workers when the window resets; an idle Claude slot during a live
  window is lost quota.
- When the workers' share of a window is spent, have each commit a green
  checkpoint and write a handoff, then continue that work with partner-model agents
  briefed from the handoff. Security reviews and second opinions stay on
  Claude: schedule them in a window that still has room.
- Keep the coordinator cheap: answer routine progress mail in one line, list
  agents with a `path_prefix`, and report to the user only state changes
  (verdicts, merges, decisions, blockers).
