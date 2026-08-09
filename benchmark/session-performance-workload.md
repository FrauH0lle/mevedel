# Controlled Session Performance Workload

Use this workload to compare the same mevedel interaction under three
instrumentation modes.  Start each run from a fresh session at the same Git
HEAD and dirty-content hash.  Approve the expected child-agent Bash request
once rather than remembering it, so later runs exercise the same interaction.

1. **Normal:** start no instrumentation, submit the prompt, and retain the
   session directory.
2. **Profiler-only:** run `M-x mevedel-telemetry-profiler-start`, submit the
   prompt, then run `M-x mevedel-telemetry-profiler-stop`.  Use this run for
   representative performance comparisons.
3. **Full debug:** run `M-x mevedel-session-debug`, submit the prompt, then run
   `M-x mevedel-session-debug` again.  Use this run for detailed request and
   view diagnosis, not throughput comparisons; gptel debug logging is
   intentionally expensive.

The child Bash request produces a permission prompt only when no covering
user-authoritative allow rule already exists.

## Prompt

```text
This is a controlled mevedel performance measurement. Follow the ordered
workload exactly. Do not optimize away, combine, reorder, or add steps.

Constraints:

- Do not modify tracked files.
- Do not commit.
- Do not use the network.
- Do not install or update dependencies.
- Use exactly two retained agents beneath root, as described.
- Use ApplyPatch exactly five times for the fixture. Do not manipulate the
  fixture through Bash.
- Run exactly the two specified root Bash calls and no other root tests or
  builds.
- Call XrefReferences exactly once, at the specified step.
- Keep working until both retained agents have settled.

1. Spawn a retained agent named `spec_review` with this exact task:

   Review the current commit against its parent for behavioral correctness.

   Your first tool call must be Bash with exactly this command, unchanged:

   pwd; git rev-parse HEAD; git status --porcelain=v1 --untracked-files=all; git diff --no-ext-diff HEAD --

   Do not add arguments, redirects, pipes, permission overrides, additional
   filesystem access, network access, or escalation. Do not run any other tool
   before this Bash call and do not run another Bash call afterward.

   After the command is approved and completes, inspect its diff against the
   documented behavior in docs/. Remain read-only, run no tests, and do not
   spawn child agents. Do not call SendMessage or send interim updates. Return
   exactly one final result containing concise actionable findings or an
   explicit statement that there are no findings.

2. Immediately spawn a retained agent named `standards_review` with this exact
   task:

   Review the current commit against its parent for repository standards.
   Read AGENTS.md and the directly relevant maintained documentation. Remain
   read-only, run no Bash commands or tests, and do not spawn child agents. Do
   not call SendMessage or send interim updates. Return exactly one final result
   containing concise actionable findings or an explicit statement that there
   are no findings.

3. Immediately call ListAgents. Do not wait for either agent and do not poll
   them yet. Continue with the fixture ApplyPatch workload while
   `spec_review` is waiting for Bash approval.

4. Immediately send this message to `/root/spec_review`:

   "After permission resolves, also verify that the retained-agent rendering
   regression covers adjacent Agent, ListAgents, and SendMessage disclosures."

5. Continue with the fixture workload. Do not call ListAgents or WaitAgent
   again until after all five ApplyPatch operations, both Bash test calls, and the
   XrefReferences call have completed.

6. Create `.scratch/mevedel-performance-measurement.md` with ApplyPatch, replacing
   any existing contents with exactly:

   # Mevedel performance measurement

   ## Baseline

   - baseline item 01
   - baseline item 02
   - baseline item 03
   - baseline item 04
   - baseline item 05
   - baseline item 06
   - baseline item 07
   - baseline item 08
   - baseline item 09
   - baseline item 10
   - baseline item 11
   - baseline item 12
   - baseline item 13

   ## State

   Initial state remains stable.

7. Perform ApplyPatch operation 2: replace the thirteen baseline-item lines with
   exactly these three lines:

   - retained item alpha
   - retained item beta
   - retained item gamma

8. Perform ApplyPatch operation 3: append exactly this block:

   ## Added observations

   - observation 01
   - observation 02
   - observation 03
   - observation 04
   - observation 05
   - observation 06
   - observation 07
   - observation 08
   - observation 09
   - observation 10
   - observation 11
   - observation 12
   - observation 13
   - observation 14
   - observation 15
   - observation 16
   - observation 17
   - observation 18
   - observation 19
   - observation 20
   - observation 21
   - observation 22
   - observation 23
   - observation 24

9. Perform ApplyPatch operation 4: replace:

   Initial state remains stable.

   with:

   The retained-agent metadata changed while adjacent collaboration results
   remained independently expandable.

10. Perform ApplyPatch operation 5: replace:

    - observation 24

    with:

    - observation 24, finalized

11. Run this as one Bash call:

    npx @emacs-eask/cli clean elc && npx @emacs-eask/cli test ert test/test-mevedel-structs.el

12. Run this as a separate Bash call:

    npx @emacs-eask/cli test ert test/test-mevedel-structs.el

13. Call XrefReferences once with identifier `mevedel-current-origin` and file
    path `mevedel-structs.el`.

14. Call ListAgents once more. If either agent is running, wait for agent
    activity using normal retained-agent coordination. Continue until both
    agents have settled.

15. Finish with a concise report containing:

    - both Bash outcomes;
    - the XrefReferences outcome;
    - the final three-participant tree state;
    - whether either review found a problem;
    - confirmation that only the ignored measurement fixture was edited.
```

## Comparison checks

- The Xref result must contain project references but no ignored
  `.mevedel/sessions/` or `.scratch/` paths and must not create a five-second
  tool span.
- In the profiler-only memory report, repeated unchanged session-sidecar reads
  should no longer be a leading allocation source after the first render.
- Normal telemetry should retain outcome-level events without detailed routine
  pipeline spans.
