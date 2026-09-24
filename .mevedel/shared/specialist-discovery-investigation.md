# Specialist discovery investigation (2026-09-21)

Task: user reports reduced use of Elisp introspection, task, and other specialist
tools after recent changes; asks how to advertise them under DEEP HARNESS rules.
Investigator: main conversation; history investigation delegated to
`/root/discovery_history`. Initially investigation only; the user's subsequent
"Let's do that" authorized the implementation recorded below.

## Confirmed observations before implementation

- Live ToolSearch: `elisp` returns 16 introspection tools; `tasks` returns five
  task-tool names without summaries. `emacs`, `plan`, and `dependencies` each
  return no matches (with category/group recovery suggestions).
- Live ToolCall successfully ran `TaskList` ("No tasks recorded.") and
  `mevedel-introspection/function_documentation` for
  `mevedel-tools--search-catalog`. This establishes representative live dispatch,
  not exhaustive tool correctness or cold-start correctness.
- `mevedel-tools.el:347-390` matches names, categories, catalog summaries and
  groups, not descriptions or full prompts. `mevedel-presets.el:491-504` builds
  catalog summaries from `mevedel-tool-summary` without a description fallback.
- Task registrations at `mevedel-tool-task.el:1152-1233` have descriptions but no
  summaries. Thus advertised dependencies/ownership in the full contracts do not
  make these tools searchable by those terms.
- Implement preset has seven native tools; Elisp, tasks and agents are
  discoverable (`mevedel-presets.el:446-460`). Discuss lacks the explicit Elisp
  group, while introspection tools only declare that group.
- `prompts/tools/toolsearch.md:5-10` frames discovery as beyond the native core
  and discourages search when a native tool covers the operation. There is no
  positive family overview, only navigation and Elisp examples.

## Interpretation and limits

- Availability is not the principal demonstrated problem in this session.
  Missing search vocabulary and absent upfront capability cues are concrete;
  their effect on spontaneous model selection remains a behavioral hypothesis.
- Lower task-tool use can also be intentional: current policy does not require
  checklists on ordinary work. More calls alone is not a success criterion.
- The investigation stage did not change product files or run ERT, paid model
  experiments, or quantitative before/after usage analysis.

## Historical evidence and recommendation

- History investigator `/root/discovery_history` identified `ab7a670` (Sep 4):
  tasks and agents moved from native schemas to discoverable groups; Elisp was
  already deferred. Main independently read that commit's message.
- `92354ac` (Sep 8) removed availability notices, post-Read/Grep specialist
  nudges, and core-tool routing advice while adopting stable native tools plus
  ToolSearch/ToolCall. Main checked the historical Elisp availability notice at
  `92354ac^:mevedel-reminders.el:1506` and selected prompt diff deletions.
- Cache rationale is documented in ADR 0111:119-133. Historical experiments do
  not establish that removing nudges preserves task performance. Main read
  `.scratch/instruction-simplification/instruction-delivery-evaluation.md:13-18,
  47-53,113-163` and `instruction-delivery-closeout.md:293-340`: selection effects
  were mixed, later runtime comparisons confounded, and caching trials supplied
  the tool schedule rather than measuring autonomous selection. No task-tool
  selection comparison was identified by the history investigator.
- Proposed first step (subsequently implemented below): small stable capability/search examples
  in ToolSearch's existing description, useful explicit summaries for built-ins,
  and suitability wording that does not treat Bash's theoretical ability to
  reproduce an operation as a reason never to discover a better specialist.
  Preserve role filtering, permissions, and the fixed native interface; do not
  restore mandatory tool routing or per-read reminders.
- Validate useful completion and evidence quality, request count, cost/latency,
  and interruptions, including small source-only controls. Increased specialist
  call count alone is not proof of improvement.

## Authorized implementation and checks

- Added a static five-family capability index and suitability-based discovery
  wording to `prompts/tools/toolsearch.md`. Added all five task summaries and
  Emacs vocabulary to all 16 introspection summaries. No search algorithm,
  role/tool roster, permission, reminder, or routing changes.
- Updated `docs/tools.md` and ADR 0111. Added a regression in
  `test/test-mevedel-presets.el` exercising the registered gptel ToolSearch
  callback with actual implement/discuss catalogs. It covers natural search
  terms, summary listings, exact contract retrieval, serialized schema stability,
  and role/request restrictions.
- Initial regression failed against the old capability description. The first
  post-change run exposed a test-only comparison issue: gptel's schema parser
  creates uninterned property keys, so Lisp `equal` fails despite identical
  provider payloads. Comparing gptel-serialized JSON tests the intended contract.
- Final Eask run: 362/362 passed across presets, tools, task, introspection,
  UI, ToolCall, and registry suites. Eask clean elc ran before tests and after
  compilation. Compilation: 206 files, no warnings/errors. `git diff --check`
  passed. Logs: `.scratch/specialist-discovery-{tests,compile}.log`.
- Independent static review by `/root/discovery_history`: no blocking findings;
  incorporated its TaskNote "Set or clear" wording correction before final checks.
- Upstream gptel refresh failed at sandboxed Git metadata despite additive
  grants. Consulted the existing checkout's tool struct, registration, schema
  serialization, and JSON APIs. Did not reload/rebuild the running Emacs install.
- No model-selection experiment performed; autonomous behavior remains unmeasured.
  Unrelated worktree edits were preserved.
