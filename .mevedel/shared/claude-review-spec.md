# Independent Claude engine spec review

Reviewed current HEAD `514ae42` against `5adfcfb28c4504968bc76b70a8bed18e8038f776` on 2026-10-07. Read-only product review; only disposable review reproductions and this report were written. The worktree changed externally from the original reviewed `f95777a` to `514ae42`; the final review uses the current product files. The parent review runs the full suite and compilation separately.

Read the local PRD, required review amendments, handoff, acceptance index, progress log, AGENTS.md, development guide, documentation and module maps, engine ownership ADR, and relevant context, compaction and session contracts. Traced admitted root and directive sends, retained-child dispatch, receipt handling, compaction rotation, cross-engine transfer, isolated text/tool workloads, Goal accounting, and native call admission. Existing passing tests and prior release claims were treated as evidence to inspect, not proof that untested paths are correct.

## Findings

### P2: Editing a retained child's transcript silently resumes different native history

Requirement: PRD §7, **“Earlier transcript edits cannot silently rewrite external history. Guard these edits or explicitly handle the divergence before further continuation.”** Also user story 27 requires context selection to preserve its advertised meaning.

Location: `mevedel-claude-code-agent.el:89-95`, with the missing observer at `mevedel-agent-conversation.el:251-335` and the root-only implementation in `mevedel-engine.el:78-84`.

Root sessions install `mevedel-engine-record-history-edit` and call `mevedel-claude-code-history-assert-current`. Child transcript buffers are writable: `mevedel-agent-conversation-open` installs no corresponding observer, and hydration sets `buffer-read-only` only when opening an inspection buffer (`mevedel-agent-conversation.el:367`). The observer itself consults only the `"root"` record. The child's runner neither checks a divergent record before resuming nor preserves a divergent state in its terminal callback (`mevedel-claude-code-agent.el:125-130`). Child records also lack a submitted-input boundary.

A user can edit the actual answer in the live retained-child transcript and send a follow-up. The displayed/persisted evidence now contains the edited answer, while `session/resume` still uses the original native answer. This can give the child instructions based on evidence it never received. Adding a guard only at dispatch is insufficient unless edits are first recorded against the correct child scope; adding a root-only modification hook to the child would also be wrong.

Reproduction: isolated Eask ran `.scratch/claude-review/spec-repro.el`, case `claude-review-spec/child-edit-silently-resumes/test`. It spawns a normal retained Claude child through the real ACP transport, waits for `ORIGINAL ANSWER`, uses ordinary `replace-match` to write `USER EDITED ANSWER`, and follows up through `mevedel-agent-control-followup`. Assertions establish that the buffer is writable, the record remains ready, the second launch resumes `fixture-session`, and the child completes while the edited answer remains in its canonical transcript.

Remedy: make transcript-edit observation scope-aware and install it for retained-child buffers; persist a child submitted-input boundary for the no-output case; assert current history before child continuation and retain `diverged` during cancellation/terminal updates. Existing explicit child excerpt recovery provides the appropriate recovery flow.

### P2: A native Goal silently charges one token when usage is unknown

Requirement: PRD §10, **“Unknown is not zero”** and **“If existing exact budget controls cannot be supported, make those controls unavailable and document actual stop behavior.”** A13 requires unsupported budget controls to be visibly unavailable. The acceptance index explicitly claims unknown usage remains unknown.

Location: newly enabled ACP accounting at `mevedel-acp-turn.el:129`, using `mevedel-goal.el:660-664` and `mevedel-goal.el:683-688`.

Goal capture still derives its fallback token estimate from the gptel `:data` payload. ACP never supplies that field. Printing nil yields three characters, so every native Goal request records a fallback estimate of exactly one token. When SDK/terminal usage is absent, shared settlement charges that fabricated estimate to the Goal and leaves budgeted continuation enabled. This is a new caller of a pre-existing fallback: the gptel path has a serialized payload, whereas the new ACP path does not. Neither input size nor emitted output contributes to this estimate.

Reproduction: the same isolated Eask file, case `claude-review-spec/unknown-native-goal-charges-one/test`, submits a 40,000-character ordinary native prompt with an active 100-token Goal. The real ACP peer completes without usage. The test establishes success, nil `:tokens-full`, nil `:data`, an estimate of one, a durable charge of one, and an active Goal. Automatic continuation is stubbed only to prevent the repro from starting another turn. The false budget accounting occurs through the ordinary send/settlement path.

Remedy: use actual native input/output evidence for a clearly labelled conservative estimate, or explicitly retain unknown accounting and stop/disable token-budget continuation when the required counters are unavailable. Do not present a placeholder one-token charge as supported consumption. Preserve ordinary unbudgeted Goal work.

Both reproductions passed their positive assertions of the bug: 2/2, 1.25 seconds ERT time, no unexpected results. Command: `npx @emacs-eask/cli test ert .scratch/claude-review/spec-repro.el`. Log: `.scratch/claude-review/spec-repro.log`. These are review reproductions, not new regression tests asserting desired fixed behavior.

### P2: Initial reminders after a completed root turn become raw Base64 user prose

Requirement: PRD §6, **“Translate ACP text, available reasoning, tools and terminal outcomes into the current canonical transcript grammar. Reuse existing view and persistence consumers”**; PRD §7 requires acknowledgement and retained delivery of guidance/Goal observations. A08/A12/A17 depend on readable, correctly classified context through restart and transfer.

Location: newly emitted receipt transcript at `mevedel-claude-code-context.el:248-254`, published by `mevedel-claude-code-context.el:391-393`; canonical repair at `mevedel-transcript.el:1081-1098`.

The native writer now preserves reminders as typed hidden audit records rather than plain reminder text. A root's prior completed turn has a fork-point audit. When canonical segmentation repairs the user prompt after that fork-point, it searches for the next assistant/tool/mail/reminder segment, but deliberately skips `ignored` segments. It then overlays the entire prefix as `user`, swallowing the new trusted reminder audit. The body is still delivered and acknowledged by Claude: the learned hash updates, the live audit record is trusted and decodable, and structural-range classification initially identifies it as ignored. The final post-fork-point repair reclassifies it as user prose. Neutral evidence contains the raw Base64 record instead of its reminder body; consumers of that canonical classification therefore lose the retained reminder interpretation. This affects ordinary second and later root turns, not merely a malformed restart fixture.

Expected-behavior reproduction: `.scratch/claude-review/root-reminder-repro.el` completes a normal admitted root turn whose real Read discovers nested instructions, changes those instructions, and submits a second prompt using `mevedel--insert-user-turn` and `mevedel--send-request`. The second turn settles successfully and its new hash plus trusted decoded audit assertions pass. The final requirement that canonical evidence contain the changed reminder exactly once fails with `:form (= 1 0)`. The retained evidence displays the Base64 block under user provenance. A clean-source isolated Eask rerun after the parent's compilation and cleanup reproduces the same failure; log `.scratch/claude-review/root-reminder-source-repro.log` (0.88 seconds test time).

Remedy: preserve validated injected-reminder audit spans while repairing a post-fork-point prompt, so root prompt recovery cannot erase canonical control records. Keep the fix in the transcript classifier and verify normal subsequent sends, reopen and cross-engine serialization; do not change receipt acknowledgement merely to make the count pass.

## Acceptance evidence assessment

“Traced” below means the production path and named evidence were inspected; it does not mean this reviewer reran all referenced tests or paid subscription runs.

| Case | Assessment |
| --- | --- |
| A01 | Traced setup, supported auth status, environment route exclusion, dependency preflight, generated wiring. Live login is Enterprise, explicitly not a separate Pro/Max account. |
| A02 | Traced root streaming, continuation/resume, FIFO queue and paired view delivery. Existing input/view tests address leading `>` drafts. |
| A03 | MCP invokes registered handlers through provider projection and shared permission/review pipeline; inspected real patch/Bash evidence. |
| A04 | Ownership checks and cancellation fence queued events/tools; transport reviewer investigates loss of acknowledged terminal usage on cancellation. |
| A05 | Current permission enforcement remains at invocation rather than discovery. Remote test exercises local Claude with remote target effects; SSH/Podman evidence is identified honestly. |
| A06 | Initial media uses ACP image blocks, output media uses MCP content; pipeline keeps persistence/render data/execution ownership. |
| A07 | Native call IDs are durable before effects and reject replay. Uncertain records require reconciliation notices; no tool replay is dispatched by ACP observations. |
| A08 | Engine refs and uncertainty survive codec load; restart fixture uses separate Emacs processes. Missing native history offers explicit excerpt recovery. Child edit divergence remains unhandled; restart also exposes the root reminder classification defect. |
| A09 | Root resumes its own native identity; directives always create a fresh conversation from selected durable evidence; child receives frozen isolated context. |
| A10 | Child runtime retains admission, capacity, follow-up, mailbox receipts and root interactions. Read-only inspection does not imply live child transcript immutability; finding above. |
| A11 | Naming/guardian/summary use isolated ACP text; Buddy/consolidation use scoped MCP tools. Parent is independently checking workload result/budget paths. |
| A12 | Context commits require exact SDK user/hook receipts. Restoration checks prevent effects before full context receipt; overflow uses a continuation prompt. The initial restoration receipt is subsequently misclassified in root canonical evidence. |
| A13 | Known per-sample usage is deduplicated and terminal totals replace corresponding counters; absent usage falls into a fabricated one-token Goal estimate, finding above. |
| A14 | Explicit guards cover declared native history operations and unsupported HTTP controls. Child transcript editing lacks the required direct continuation guard. |
| A15 | Full regression/compilation rerun belongs to parent. Prior evidence alone does not resolve the new repros. |
| A16 | Live Eventfold artifacts record 80 calls, multi-file patch, Bash tests and image read. Original driver failed; acceptance index correctly distinguishes later offline verification. |
| A17 | Inspected actual gptel dry-run assertions for effective segment, safe reasoning and redelivered instruction state; detachment persists before HTTP dispatch. |
| A18 | New native conversation starts from labelled effective excerpt, preserves archive/effect chronology; return switch and saved resume tested. |
| A19 | Root/child advertise compaction, rotate through existing transactions, reset stream markers and keep admitted turn ownership. Completed identities deduplicate terminal rotation. Directives do not advertise rotation of the containing root segment; synthetic tool observations are ignored and the context hook still runs. |
| A20 | Aliases plus ACP metadata catalog; configured unavailable IDs fail before prompt dispatch. First-use effort fallback is visible and acknowledged, rather than a silently ignored initial SDK option. |
| A21 | Named deterministic model-switch test and retained live Sonnet-to-Opus evidence establish a reused native ID and new selected model. No paid rerun conducted by this reviewer. |

## Scope and limits

No independently established scope-creep finding. The standalone old-session converter conflicts with the initial no-migration PRD wording, but the retained progress log records a later explicit user-requested narrow exception; it should not be reported as unauthorized work without contrary conversation evidence. Likewise Fable and visible unsupported-effort fallback are recorded first-use follow-up decisions. Current maintained contracts should explain those amendments where necessary.

This reviewer did not invoke credentials, make subscription/API calls, change product code, or merge/commit anything. Additional findings from other reviewers remain independent of this report. The three confirmed paths prevent a zero-gap Spec verdict even if the complete existing suite remains green.

## Restart suite verification follow-up

The parent reports a complete canonical run of 9,406 cases, one unexpected result and 24 skips. The failing separate-Emacs restart test retains a raw `how-many` assertion for instruction bodies, despite their newly Base64-encoded persisted form. Replacing only that assertion in a disposable fixture makes phase 1 pass, but a decoded-evidence check fails for the root in phase 2. Independent diagnostic runs confirm native restoration succeeded: phase-2 hashes match the new contents and trusted audit decoding contains the phase-2 body; child evidence also decodes it. The final root segmentation alone swallows the new record after its prior fork-point. The ordinary two-turn reproduction above establishes the product defect without relying on the stale raw assertion or on restart-specific direct input insertion. Do not report the suite as green or conflate correcting the obsolete assertion with fixing canonical root context.

## Implemented corrections and focused verification

The user subsequently authorized fixing every finding. The child history and
canonical reminder findings above are now corrected in this worktree; their
original descriptions retain the reviewed baseline evidence. Goal accounting is
being corrected independently by the parent agent.

Retained child buffers install the shared, scope-aware edit observer. Native
child dispatch checks divergence at entry, launch and identity admission. Each
child records a submitted-input body offset with private segment identity zero;
compaction rebases that offset inside the transcript/state publication, so the
new summary and its boundary are committed together. Both successful and
interrupted settlement preserve divergence. Unsent drafts and normal metadata
publication remain editable without poisoning native history, and explicit
excerpt recovery still sends edited evidence into a fresh native conversation.
Root and child scopes remain independent.

Canonical post-fork-point prompt repair now reapplies authoritative structural
ranges after correcting stale user-prompt properties. The new ordinary two-turn
root regression verifies native acknowledgement, instruction hashes, trusted
receipt decoding and decoded evidence. The cold restart fixture counts decoded
evidence rather than encoded raw bytes; its separate-editor phases pass.

Focused source-only Eask evidence:

- `.scratch/claude-review/spec-fix-initial.log`: 30/30 passing, including the new
  root reminder test, existing child limits and cold restart.
- `.scratch/claude-review/spec-child-fix.log`: 3/3 passing. The edit/recovery case
  covers completed children, native compaction with no post-summary output,
  active no-output cancellation, and reopened no-output children hydrated
  through the normal loader; it verifies draft editing, continuation refusal,
  root scope isolation and explicit excerpt recovery. A separate case verifies
  success cannot clear divergence recorded during the admitted turn.
- `.scratch/claude-review/spec-fix-final.log`: intermediate combined run,
  151/153 passing. Existing root, restart, recovery, transcript and multiline
  composer-draft compaction regressions passed. The two failures were the new
  transport busy-abort regression (transport agent subsequently reports 15/15
  for its turn/compaction run) and a reopened-child test setup that seeded fake
  root metadata before restoration (corrected; final child suite above passes).
  The parent's final full-suite/compile run owns the aggregate verdict.

No credentials, paid requests, commits or compatibility paths were introduced.

## Additional recovery finding and resolution

P2 requirement: PRD user story 38 requires retained compaction summaries so
**"later continuation uses the effective history"**; §9 requires labelled
summary or selected-excerpt continuation of the effective transcript.

`mevedel-claude-code-history-excerpt` originally passed the whole effective
segment to `mevedel-transcript-project-evidence`, whose segmentation
intentionally skips the leading root summary. Public root recovery after native
compaction therefore discarded the entire summary even without an edit. A child
summary was not skipped wholesale, but ordinary query-replace of its final text
could inherit `gptel=ignore`/front-sticky from the closing wrapper, causing the
edited body to vanish from the same recovery evidence. This was a supported raw
edit, not an artificial provider operation.

The native history owner now reads the authoritative root/child summary bounds,
projects that body independently of incidental gptel properties as labelled
compaction-summary evidence, and excludes its original range from ordinary
projection so it appears once. It strips private hook-audit scaffolding using
the same existing summary handling. Archive retention, exact-resume refusal,
explicit recovery and view segmentation remain unchanged.

`.scratch/claude-review/spec-summary-red.log` records a failing normal root
compaction/recovery/send regression before this fix: even the unedited summary
never reaches the fresh conversation. The final recovery regression covers both
unedited and query-replaced root summaries. The child compaction regression now
again replaces the complete final summary text and verifies the edited body in
the explicitly recovered native conversation.

`.scratch/claude-review/spec-summary-green.log` records 10/10 source-only Eask
cases passing after the correction: root summary recovery (unedited and complete
final-text query replacement), child full-summary replacement/recovery, retained
child lifecycle cases, existing explicit recovery guards/fault handling, and both
engine-transfer directions. The summary occurs exactly once in the recovered
root model input and archived original evidence is absent. No test subprocesses
remain pending from this reviewer.
