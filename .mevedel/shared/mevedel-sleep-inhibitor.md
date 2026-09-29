# mevedel: sleep-inhibitor backlog item — investigation (2026-09-21)

Backlog item `docs/backlog.md` → "Prevent system sleep during active requests"
was investigated on request. Full report: `.scratch/sleep-inhibitor/report.md`
(workspace-relative; gitignored).

Key facts a follow-up session needs:

- No platform code is needed. Emacs 31.1 (already mevedel's minimum,
  `mevedel.el`:9) bundles `system-sleep` with logind/NS/w32 back-ends:
  `system-sleep-block-sleep` / `system-sleep-unblock-sleep`; pass
  `allow-display-sleep` = t to keep screen blanking and locking unaffected.
- Verified locally: back-end `dbus`; logind lock shows `WHAT sleep MODE block`;
  explicit unblock removes it; a SIGKILLed Emacs releases it (OS-side fd
  release); acquisition ≈ 5.8 ms.
- Side effect: loading `system-sleep` installs a permanent logind delay
  inhibitor ("Emacs sleep event watcher", up to 5 s suspend delay), so load it
  lazily behind a gate.
- Lifecycle facts: root/fork requests own a `mevedel-request` and teardown
  funnels through `mevedel-request-cancel` (mevedel-turn.el); agent turns own
  **no** request (agent buffers get `mevedel--session` but not
  `mevedel--current-request`), so they need their own bracket around
  `mevedel-agent-runtime-dispatch` → `mevedel-agent-runtime--finalize`.
- Traps: `mevedel-skills--preparation-settler` creates a temporary request
  directly (never released by request teardown), so acquire in
  `mevedel-request-begin`, not in `mevedel-request--create`; release via
  `mevedel-request-push-canceller` for exactly-once semantics.
- Open scope questions: interactive pauses (`active-work-pause`), yielded Bash
  executions (ADR 0024, owner-scoped, outlive requests), auxiliary provider
  calls (naming/summary/memory-review/permission-review/buddy/compaction).
