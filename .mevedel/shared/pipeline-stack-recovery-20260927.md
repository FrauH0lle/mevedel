# Pipeline stack exhaustion and live recovery

Task: user's reported `excessive-lisp-nesting 1601` while the spinner Goal
delivered a ToolCall for `mevedel-introspection/symbol_exists`.

## Observations

- The live pipeline and PTC driver were interpreted source. The pipeline's
  loaded source resolves to this checkout through Straight's build symlink;
  no pipeline bytecode existed in that build directory.
- The request was stranded in gptel's TRET state, with a tool result already
  recorded, no live provider process, and no pending turn settlement. Replaying
  TRET would risk repeating partial prompt/result mutations.
- The runner recursively invokes NEXT. Its existing interactive time budget
  does not bound stack usage for fast steps. An 80-step regression with a
  60-second time budget and the unchanged 1600 evaluator limit failed after
  only 74 steps on the original runner.
- There is no captured stack trace of the original incident. The regression
  proves the runner's unbounded fast-chain failure, not every frame involved
  in the original provider callback.

## Change and evidence

- Interactive chains now yield after eight runner frames, retaining existing
  cancellation and error boundaries. Batch chaining is unchanged.
- Deferred execution waits for active runner frames to unwind and binds its
  own depth throughout resumed execution. Independent review caught the
  missing resumed binding in the first candidate; a regression reproduced
  that reentry before correction.
- Four new regressions cover long fast chains, process waits in both initial
  and timer-resumed predecessors, and cancellation of a stack-deferred step.
- Final focused pipeline/PTC run: 298/298 passing. Broader transport/tools/
  render-data/Goal/chat run: 265/267 passing. Both failures also reproduce
  with HEAD's original pipeline: `mevedel-tools--handle-message-inject/test@2`
  and `mevedel-tools--handle-steering-inject/test@9` (reasoning/injection
  ordering assertions). They are not fixed by this change.
- Independent read-only follow-up review found the confirmed issue resolved
  and no remaining concrete blockers. No full-suite success is claimed.
- Final Eask compilation: 210 files, no warnings or errors; Eask bytecode
  cleanup afterward. `git diff --check` passed.

## Runtime recovery

Installed only the changed runner definition and its new depth variable in
live Emacs. `max-lisp-eval-depth` remains 1600. Used normal `mevedel-abort` to
retire the stranded request, confirmed no request or pending settlement, then
resumed the Goal with an explanation. Existing implementation edits and commits
were preserved. The CPU capture remains active.

The Goal agent was told to preserve this recovery task's changes in
`mevedel-pipeline.el`, `test/test-mevedel-pipeline.el`, and `docs/tools.md`, and
to report a real physical-battery blocker rather than repeatedly polling it.

Live follow-up completed normally: Bash, ToolSearch, and nested GetGoal /
UpdateGoal calls returned, the turn settled, and no request remained. The Goal
is now explicitly blocked on unplugging the laptop for the physical battery
comparison and using a fresh Emacs for the new animation's live-stream check.
This is separate from the repaired callback hang. Profiling remains active
under the previously agreed completion/manual-stop policy.
