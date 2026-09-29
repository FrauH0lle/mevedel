# Source progress after accepted-plan handoff

2026-09-27, task: stale preparation row in
`*mevedel:Background Tool Call UI/UX@mevedel:view*`.

- Live observation: source Plan metadata was accepted, Worktree/Fresh/Goal,
  with no implementation retry; its view still owned `plan-preparation` and
  had a running spinner timer. Loaded handoff library was under Straight's
  build directory.
- Fix in `mevedel-plan-handoff--dispatch-submission`: release only the source
  view's preparation-owned spinner before Goal construction/request kickoff.
  No changes to the concurrent animation implementation were needed.
- Regression covers Here/Worktree, Direct/Goal, unrelated spinner ownership,
  a multiline leading-`>` composer draft and point, and timer cleanup. The
  fixture displays the view because hidden views intentionally do not animate.
- Validation: warning-free Eask compile; Eask cleanup then 231 passing tests
  across plan-handoff, plan-mode, and view-stream. Upstream gptel refresh was
  blocked by read-only Git metadata; existing local source was consulted.
- Live repair: cleared only the stale preparation spinner after checking the
  retry was absent; timer stopped and draft stayed unchanged. The changed
  handoff module has not been reloaded into live Emacs or rebuilt by Straight.
