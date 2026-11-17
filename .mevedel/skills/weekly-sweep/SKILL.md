---
name: weekly-sweep
description: "Spend the weekly subscription quota left before its reset on an unattended sweep: architecture deepening and backlog fixes with a proxy user, plus test-suite upkeep. Use when the user starts the weekly sweep or resumes one."
argument-hint: "Optional: track split or focus areas"
---

# Weekly sweep

An unattended run that turns quota which would expire at the weekly reset into
reviewed branches. The user is away after the intake. You are the coordinator:
you dispatch, review, measure and integrate; workers implement.

The architecture track runs this skill unattended, with the overrides in
[ARCHITECTURE-TRACK.md](ARCHITECTURE-TRACK.md):

!$improve-codebase-architecture

## Run rules

- **Fix what you meet** in the area you touch: flakes, coverage gaps, stale
  docs.
- **Re-read the backlog at every dispatch and review.** Read all of the main
  checkout's `docs/backlog.md`, Inbox included; it can hold the user's
  uncommitted entries, so workers read it there, never in their worktree. The same holds for this
  skill's files, which the intake and close update there uncommitted. A
  matching entry joins that workstream's brief. Workers report the entries
  they resolved; the coordinator removes an entry only when a merged commit
  demonstrably fixes it, and annotates it otherwise. Backlog text never cites
  `work://shared/` notes, which expire; put the needed facts in the entry.
- **Decide in the user's name.** The proxy user applies
  [DECISION-RULES.md](DECISION-RULES.md); see
  [ARCHITECTURE-TRACK.md](ARCHITECTURE-TRACK.md) for the second opinion and
  **disputed** decisions (the proxy and second opinion disagreed; the more
  reversible option was taken). Every decision goes into `decisions.md`
  beside the ledger: question, options, choice, rule or source, confidence.
  The questioner writes grilling decisions; the coordinator writes the rest.
- **Review afterwards, by default.** ADR amendments and new ADRs, policy text
  in `docs/development.md`, `AGENTS.md` or `CODING_STANDARDS.md`, and behaviour
  changes proceed on their branch; so do disputed decisions. List them under
  "Review afterwards" in `overview.md`. **Escalate** only these to
  `needs-user.md` beside the ledger, with options, evidence and a
  recommendation, and continue with other work meanwhile: removing a user
  feature, changing a security boundary, and reversing an ADR's decision
  (choosing what it rejected or dropping its decision; refining or extending
  it is an amendment). The coordinator writes `needs-user.md`, including
  questions the proxy answered `NEEDS USER`, which the questioner forwards.
- **Branches only.** Each workstream gets branch `sweep/DATE/SLUG` in worktree
  `.scratch/sweep-DATE/SLUG` (bootstrap with `scripts/bootstrap-worktree.py`).
  DATE is the sweep's start date, `YYYY-MM-DD`.
  Integrate on `sweep/DATE/integration` (Git cannot hold `sweep/DATE` beside
  branches under it). `master` stays untouched until the close.
- **One merger.** Once an integrator agent exists, only it merges into the
  integration branch; the coordinator sends it the branch.
- **The ledger is the run's memory**: `work://shared/weekly-sweep/DATE/ledger.md`.
  Record every dispatch, result, quota reading, measurement and decision there,
  so a fresh session can resume from the ledger alone. Stamp entries with the
  time from `date` or the latest quota reading, never an estimate. Keep `overview.md`
  beside it for the user: one table per track (workstream, result, status) and
  the "Review afterwards" list, refreshed after merges.

## Safeguards

- The test runner raises its own OOM score, so the kernel kills test workers
  before Emacs. Prefix other heavy commands (benchmarks, one-off probes) with
  `choom -n 1000`. Bound every command with `timeout`: `30m` for focused work,
  `60m` for a full suite or fresh-file lane (a full suite at `--jobs 2` once
  ran past 30 minutes). Before the close, check `pgrep -af emacs` for strays
  from finished agents.
- Focused runs use `--jobs 4` at most. Exactly one full suite runs at a time,
  started by the coordinator or the integrator.
- Contention checks (several copies plus a CPU hog pinned to one CPU) each get
  their own CPU from the coordinator: the reviewer CPU 0, workers one each
  counting down from the second-highest CPU (CPU 30 on 32 CPUs).
- At most eight test-running agents on this 32-CPU, 62 GB machine; scale down
  on smaller machines. Read-only agents such as the proxy user do not count.
- A worker may split its work across helper agents on disjoint files. Its
  brief says the helpers report to the worker, and the worker tells them so;
  only the worker reports to the coordinator (helpers asked the coordinator
  for go-aheads five times on 2026-11-14).
- Spawn every worker with `working_directory` set to its worktree; agents
  inherit the session's cwd (the main checkout), and relative edits landed
  there twice on 2026-11-07.
- Prepare each worktree with `python3 scripts/bootstrap-worktree.py DIR`, which
  also installs the pre-commit hook. When it reports missing dependencies,
  link the main checkout's `.eask` with an absolute symlink and rerun it.
- No `git stash` in sweep worktrees: the stash is shared by every worktree of
  the repository, and on 2026-11-14 one worker popped another's stash. Base
  controls use a throwaway worktree or `git show REV:FILE` copies.
- Workers commit with the pre-commit hook on. Briefs export
  `MEVEDEL_TEST_JOBS=4` and `MEVEDEL_AFFECTED_BASE=<branch-base-sha>`, so the
  hook's `check -- --affected` (standards, commit message, strict compile,
  affected tests) stays bounded and compares with the branch base. After only
  rewording a checked commit's message, run `python3 scripts/check-standards.py`.
  One full affected run on the final tree is enough; the integrator's merge
  check repeats it. Never `pkill -f` a pattern shared with other agents' runs; a
  worker that kills a test run kills its whole process group.
- During a planned pause (spent window), stop the reminder Monitor, restart
  the watchdog with `--heartbeat 50` (pauses over an hour; see
  [BUDGET.md](BUDGET.md)), and arm one wake timer for the window reset. The
  watchdog prints only state changes, so a healthy run wakes nobody.
- A Monitor lives at most one hour: start the watchdog and the reminder with
  `timeout_ms` 3600000, record their expiry in the ledger, and re-arm each
  before it expires. The wake timer is a yielded Bash `sleep` whose
  completion wakes the coordinator.
- Agents that serve repeated requests (reviewer, proxy user, second opinion)
  end their turn between requests: an agent waiting in `WaitAgent` holds an
  active-turn slot. Send the next request only once `ListAgents` shows it idle;
  mail to a running agent can be dropped. Every `WaitAgent` uses a timeout of
  20 minutes or less.
- Live `Eval` belongs to the coordinator and returns only scalars or short
  strings. Backend objects carry credentials; never return or print them.

## Steps

1. **Intake**, while the user is still there. Ask at most eight questions in one
   `Ask`:
   - the **target subscription** whose weekly quota the sweep spends:
     propose the preset's provider and show both subscriptions' weekly
     usage and reset times ([BUDGET.md](BUDGET.md)). If they pick the other
     one, recommend restarting the sweep in a session with that provider's
     preset, so the coordinator and workers draw from it;
   - focus areas and features not to touch;
   - whether work may continue past the weekly reset (default: hard stop);
     if so, until when;
   - the track split, if not the default;
   - the **partner model**: propose the strongest model of the other
     provider than the current preset, from the backends actually
     configured, and ask them to confirm or name another; also any specific
     models they want elsewhere;
   - the "Unsettled" items in DECISION-RULES.md, which include the previous
     sweep's disputed decisions.
   Fold their answers into DECISION-RULES.md and the ledger.
   Done when the ledger holds the intake answers.
2. **Budget.** Follow [BUDGET.md](BUDGET.md): read the quota, write the window
   schedule, cutoffs and reserve into the ledger, and start the reminder
   Monitor.
   Done when the ledger holds the schedule.
3. **Watchdog.** Get Emacs's PID with `(emacs-pid)`, then start a Monitor
   (unconfined, it must see Emacs) running
   `.mevedel/skills/weekly-sweep/watchdog.sh PID`. On `PRESSURE` or `STALL`,
   pause dispatch and reduce active workers.
   Done when the first `OK` line arrives.
4. **Resume or start.** If the ledger has workstreams, continue its open ones.
   Otherwise create them from [ARCHITECTURE-TRACK.md](ARCHITECTURE-TRACK.md),
   [TEST-TRACK.md](TEST-TRACK.md) and a **backlog bugs** workstream for
   `docs/backlog.md` defects no other workstream covers. View and composer bugs
   need verification of the rendered result in a real view, not just internal
   state.
   On a fresh start, create `sweep/DATE/integration` from `master` in its own
   worktree and spawn the integrator (balanced tier) there.
   Done when every workstream has a ledger row with status and owner.
5. **Dispatch** in ledger order until the dispatch cutoff, keeping worker slots
   full. Split worker slots 75% improvements (architecture and backlog bugs)
   and 25% tests, unless the intake or argument says otherwise. Pick each
   agent's tier from the current preset by the difficulty of its task, unless
   the user named models:
   - **strong**: architecture exploration, the questioner, the second
     opinion, security reviews and hard debugging;
   - **balanced**: ordinary implementation and test fixes;
   - **fast**: mechanical edits, timing triage and lookups.
   The **partner model** chosen at intake, from the other provider and on its
   own quota, is the proxy user and the diff reviewer, and continues work when
   the preset's quota window is spent. Security reviews go to a Claude model,
   whichever role it has: OpenAI's cyber-policy filter stopped a GPT review
   mid-way.
   There is no fixed implementation model: pick each from the configured
   backends (`BACKEND:MODEL` in `Agent`) by task.
   Each brief states the problem in one sentence, a size estimate, owned
   files, the worktree path, the track file's address, the ledger address,
   and the compact result to return. Workers stop and report when the work
   grows well beyond the estimate.
   Done for a workstream when its worker returns a result.
6. **Review** each result: the partner model checks the diff against
   `CODING_STANDARDS.md`; you check the evidence yourself. Rerun the focused
   tests and re-measure claimed speed-ups under the same load. Every new
   watchdog, bound or injection needs a negative control showing that it fires
   (a timer inside `accept-process-output` with JUST-THIS-ONE never does).
   Send all findings, non-blocking included, back to the same worker. After
   each merge into integration, the integrator runs the `--affected` tier
   there: parallel branches can conflict in signatures Git does not flag.
   Done when focused tests pass, the reviewer reports no blocking findings,
   and the ledger row records branch, commits and measurements.
7. **Close** at the dispatch cutoff: dispatch nothing new.
   - List `git branch --no-merged master --list 'sweep/DATE/*'` and account
     for every branch: merged into integration, or recorded as pending with
     its reason.
   - Run the full suite once with nothing else running and compare wall time
     and total test-seconds against the baseline.
   - Write the ledger's report: per branch, what changed and its
     measurements. Finish `overview.md`, its "Review afterwards" list, the
     disputed decisions, and `needs-user.md`.
   - Copy each disputed decision, in one line with its options, into the
     "Unsettled" section of DECISION-RULES.md. Run the closing full suite
     from the main checkout, or copy its report directory into the main
     checkout's `.scratch/test-suite-performance/` (worktrees are removed),
     and record its name in TEST-TRACK.md as the next baseline. Shared notes
     expire after 14 days; the skill directory and `.scratch` reports persist.
   - Fast-forward `master` to the integration tip once both merge-time gates
     pass ([checks table](../../../docs/development.md#checks-before-committing));
     preserve the user's uncommitted edits in the main checkout. Then commit
     this skill's updated DECISION-RULES.md and TEST-TRACK.md on `master`.
   Done when the report is written, every branch is accounted for, and every
   worker is finished or its exact state is in a handoff before the hard stop.
