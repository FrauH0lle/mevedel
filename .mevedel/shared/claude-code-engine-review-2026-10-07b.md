# Claude engine: second review and fixes (2026-10-07)

Independent review of `5adfcfb..836de68` after the first review round, with
fan-out reviewers for ACP transport, MCP bridge, Claude adapter, context and
persistence, shared workflow modules, over-engineering, tests/docs, and the
browser collaboration stack (relay, viewer, shared editing). A second pass
reviewed the fixes themselves. All findings below are fixed in the commit that
adds this report unless listed under "Open".

## Fixed

Correctness (P1):

- The ACP filter wrapper let-bound `timer-list`, so every `cancel-timer` from
  ACP handlers silently failed. Timers are now held only while TRAMP-nested
  (diffing a copied list; a shared list missed spliced timers).
- gptel composer sends lost their prepared model input and could drop a
  skill's permission stash (double dispatch through the `gptel-send` advice).
  The advice passes through during an engine dispatch; composer input reaches
  only external engines, so gptel prompts are unchanged from master.
- The MCP socket was reachable from sandboxed Bash; sockets now live under one
  private root masked with `--tmpfs` in every local bubblewrap profile.
- Stale directive/unstarted records permanently refused Fork, Rewind, Save As,
  control transfer, compaction and `/btw`; only native identities count.
- MCP tool calls could overtake ACP frames read in the same pass and fail the
  turn with an unacknowledged-context error; socket work queues behind the
  drain timer.
- Child results over 10k characters never reached a native root (WaitAgent
  loop); oversized single mail takes the continuation route.
- Shared-item questions in Claude sessions resumed the room conversation and
  lost item framing and the untrusted-artifact guard; they now run isolated
  with labelled same-item evidence.
- Refused guest queued messages wedged the follow-up queue; they are dropped
  with a generic notice, and transient busy/compaction states wait instead.

P2 and smaller: MCP filter reentrancy and quadratic buffering; lost replies on
serialization or publication failure; JSON null parity; dead-process ACP
respawn; events lost after cancel (completed compaction); hooks run after their
client gave up; 51 ms synchronous sidecar publish per tool call (ledger
removed, ADR 0123 amended); unbounded model-catalog growth from guest strings;
user `defaultMode` leaking into sessions; `auth status --json`; wire markers and
untyped observations in transcripts; re-sent acknowledged instructions and
selected context; directives acknowledging root instructions; Read dedup after
any compaction (both engines) plus the native omitted-files reminder; child
tool calls driving the root view and missing running cards for guests (native
calls now run gptel tool hooks); lost collaboration publish timer under TRAMP;
interrupted-child excerpt dropping edited input; agent-runtime timer leak;
abort save nesting inside remote operations; `tool-error` activity never
recorded.

Simplification: removed the native call ledger, `listChanged`, the AIR
`sessionFailure` branch, the session-info slot, `models-discovered-p`, dead
cleanup branches and duplicated root/child plumbing; extracted shared helpers
(hook size, reminders turn warnings, native tool roster, shared-item prefix).
Docs consolidated to one owner per contract; ADR 0115 reduced from 379 to 112
lines. Test infrastructure: 66 of 71 hand-built launch stubs now run the real
launch plist (key deletions now fail 5-23 tests), and the engine fixture fails
on leaked timers.

## Verification

- Full isolated suite: 9,470 tests, 0 unexpected, 23 conditional skips.
- `eask compile`: 229 files, no warnings (in a disposable copy).
- Relay `go test`, collaboration JS tests and `npm test --prefix shared-editing`
  (51/51) pass; browser/Playwright suites and remote acceptance were not run.
- No paid model calls were made.

## Open

- Item framing is still replaced when an item question carries a real model
  input rewrite (skill or UserPromptSubmit hook); pre-existing on master.
- Guests: newly discovered Claude models do not reach already-connected
  viewers; excerpt continuation is not announced to guests; a lobby delete
  leaves Claude's native transcript on disk; guest PDFs become a placeholder.
- A Claude session whose native history lives on the other machine cannot be
  continued from the lobby until recovered in Emacs, and nothing in the lobby
  says so.
- `UpdateGoal` stays offered with no active Goal (roster is fixed per turn).
- Raw SDK message mirroring (`emitRawSDKMessages`) duplicates stream traffic;
  narrowing it needs a live check.
- The branch is behind master (collaboration changes) and needs a merge.
