# Manual memory-quality evaluation

These opt-in cases exercise real context-summary generation, immutable digest
publication, and ordinary journal Grep. They cover a user correction, a resolved
test failure, an abandoned decision, a buried lesson, repeated summaries, and
continuation with a missing or failed journal. No network call runs in the
ordinary test suite. The fixtures are synthetic; expectations are kept outside
the model input.

`run.el` uses a configured DeepSeek or Codex backend. It accepts a temporary private
provider configuration, creates a temporary workspace through Eask's isolated
test environment, and writes evidence under `.scratch/memory-quality/` by default.
`MEVEDEL_QUALITY_OUTPUT` chooses another output directory. Model credentials
remain in a mode-0600 temporary file and are deleted when the test settles.
If loading fails before the test starts, remove that temporary file explicitly.

Evaluate the first form in `export-provider.el` in an existing configured Emacs.
For example, replace `/ABS/REPO` below with this checkout's absolute path:

```elisp
(with-temp-buffer
  (insert-file-contents "/ABS/REPO/test/manual/memory-quality/export-provider.el")
  (eval (read (current-buffer)) t))
```

This returns only the temporary filename. It reads the existing backend and
workload selection without loading branch code into that Emacs. Then run:

```sh
npx @emacs-eask/cli clean elc
MEVEDEL_QUALITY_PROVIDER=/tmp/RETURNED-PROVIDER-FILE.el \
  npx @emacs-eask/cli test ert test/manual/memory-quality/run.el
```

Optional environment variables:

- `MEVEDEL_QUALITY_GPTEL`: source directory for the dependency being evaluated;
  newer source is preferred over stale bytecode.
- `MEVEDEL_QUALITY_MODEL`: a model already declared by the exported backend,
  such as `deepseek-v4-pro`, for an explicit comparison. This does not change
  the user's workload configuration.
- `MEVEDEL_QUALITY_RESULTS`: output subdirectory, default `results`, so a
  comparison does not replace earlier evidence.
- `MEVEDEL_QUALITY_DIGEST_EFFORT`: explicit comparison effort for digests;
  continuation keeps the exported policy. For the GPT comparison, `none`
  corresponds to the DeepSeek comparison's disabled digest reasoning.

All requests now use unmodified production admission. The initial Codex
comparison needed an explicit test-only exception because the implementation
incorrectly required every provider to expose a token control. That exception
has been removed. Production now retains the input reserve, caps supported
server controls, and applies the digest byte limit and client deadline even
when Codex has no server token control. This does not guarantee a server token
or billing ceiling. Historical reports preserve the initial run conditions.
The failed-journal case uses a one-byte digest allowance and requests one output
token where supported, so a real callback fails validation on either transport.

To export Codex access, dynamically bind `process-environment` around the export:

```elisp
(let ((process-environment
       (cons "MEVEDEL_QUALITY_BACKEND=Codex" process-environment)))
  (with-temp-buffer
    (insert-file-contents "/ABS/REPO/test/manual/memory-quality/export-provider.el")
    (eval (read (current-buffer)) t)))
```

This exports temporary authentication headers, without exporting refresh tokens
or changing the live workload. Use a separate export per run; the harness deletes
each file. Select each exact registered model with `MEVEDEL_QUALITY_MODEL`.
The request harness has a network-free timeout/cancellation check:

```sh
MEVEDEL_QUALITY_GPTEL=/PATH/TO/GPTEL \
  npx @emacs-eask/cli test ert test/manual/memory-quality/request-test.el
```

Outputs include the prompt hash, actual model/effort, provider token usage,
configured price rates, full digest or continuation, retrieval query/result,
and frozen input. Input counts exclude cached input, which is recorded
separately. Price rates are configuration values, not a billing receipt.
Syntactic validity and a literal match are necessary checks, but do not prove
factual retention or correct provenance. The user accepted the initial model
comparison and selected Sol provisionally for summarization. Keep that decision
distinct from the remaining proposal-quality and application evaluation.

The initial Flash runs demonstrated both reasoning-budget exhaustion and
all-empty digests that omitted decisive facts. A Pro comparison passed all seven
extraction/retrieval checks. Subsequent Luna, Terra, and Sol comparisons informed
the user's selection; Sol also passed the seven production-path cases.
Preserve failing runs when changing prompts, scope, or models. Do not repeatedly
rerun an unchanged case until a lucky result is treated as acceptance.

`consolidation.el` extends the seven cases through the real buddy review and
checked application. It consumes the accepted stage-one digest artifacts for
the five extraction cases. Missing/failed-journal cases contain no digest and
review current memory through the explicit empty-batch path. Existing memory
deliberately contains stale claims in the correction, resolved-test,
abandoned-decision, and repeated-summary cases. Review questions and expected
answers stay outside model input.

Export the configured buddy workload by binding
`MEVEDEL_QUALITY_WORKLOAD=buddy` in `process-environment` around the export form.
Then run:

```sh
MEVEDEL_QUALITY_PROVIDER=/tmp/RETURNED-PROVIDER-FILE.el \
MEVEDEL_QUALITY_OUTPUT=/ABS/QUALITY-OUTPUT \
MEVEDEL_QUALITY_DIGESTS=/ABS/ACCEPTED-STAGE-ONE-RESULTS \
MEVEDEL_QUALITY_RESULTS=consolidation-buddy \
  npx @emacs-eask/cli test ert test/manual/memory-quality/consolidation.el
```

Each case runs once in manual mode and once in auto mode. Manual mode simulates
acceptance inside the temporary fixture to exercise application; it does not
claim human semantic approval. Reports include admitted input, complete reply,
resulting topic/index files, decision statuses, usage, elapsed time, and prompt
hash. Instruction proposals stay pending. Technical success is separate from
semantic review; unsupported or superseded guidance fails the quality review
even when the request and file writes succeed. The network-free harness check is:

```sh
npx @emacs-eask/cli test ert test/manual/memory-quality/consolidation-test.el
```

The inspected gptel branch (commit `4573f93`) incorrectly sends
`reasoning_effort: "disabled"` to DeepSeek. The service rejects it. The separate
patch here omits that field while retaining `thinking.type: "disabled"`, matching
[DeepSeek's API](https://api-docs.deepseek.com/api/create-chat-completion/).
It was tested in an isolated clone; it has not been applied to the user's live
installation. Its independent, network-free payload check is:

```sh
emacs --batch -Q -L /PATH/TO/PATCHED/GPTEL \
  -l test/manual/memory-quality/gptel-disabled-test.el \
  -f ert-run-tests-batch-and-exit
```
