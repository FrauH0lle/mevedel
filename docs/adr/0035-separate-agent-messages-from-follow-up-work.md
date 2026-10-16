# Separate agent messages from follow-up work

Status: accepted. Incorporates ADRs 0046, 0047, 0048, 0049, 0050, 0051, and 0052.

## Current decision

SendMessage queues nonempty plain-text information. Agent-originated MAIL to
`/root` can start an idle Background event turn through the Monitor scheduler;
root self-mail and mail to non-root recipients do not start turns.
FollowupAgent gives an addressable non-root agent work: it starts an idle
conversation or steers an active one at its next safe boundary. The same text
shape applies to Agent's initial task. Shared files can carry larger referenced
material; there is no structured attachment or dual text/item protocol.

Successful Agent returns the canonical child path. SendMessage and FollowupAgent
have empty successful model results; the original calls already retain target
and text. Invalid targets, exhausted capacity, and dispatch/delivery failures
remain tool errors. Observation/control tools retain their own result shapes.

Unread mail is durable root-session state and is delivered FIFO before the
recipient's next sample, independently of WaitAgent. A communication record
carries type, sender/recipient paths, and payload. Once delivered it lives in
conversation history rather than the unread queue. There are no model-facing
message IDs, replay controls, or acknowledgements.

Every settled child turn stages one terminal RESULT for its original spawn
parent, including later turns initiated by a peer. An ordinary RESULT to `/root`
can wake the idle root; it does not activate a non-root parent or broadcast.
Outcomes are completed, errored, or interrupted; payloads
carry the final response, concise failure, or interruption reason with useful
partial text. A workflow awaiting its child may consume that exact committed
record through its result handler without waking the root. Handler failure
leaves it for ordinary mail, including idle-root wake eligibility.
A yielded Bash completion that arrives while the child still works reaches the
child itself, as `EXECUTION` mail read at its next provider request, like the
root's own completions. Completions the child can no longer read, because they
arrive after its final answer or remain unread at settlement, are committed
alongside that result as separate `EXECUTION` records to the spawn parent, with
the child as sender. They do not alter the final-response payload or the
workflow result handler's input.

Idle-root wakes share Monitor's scheduling order, holds, and current root
permissions; they are not user instructions or new authority. An active
WaitAgent still resumes through its existing callback rather than starting
another turn. Wake marks are runtime-only: restart and recovery do not turn
saved mail or recovery-generated results into background work. Successful root
turns reoffer eligible unread mail that arrived after their last sample; failed
or aborted turns do not automatically retry it in a wake loop.

Inline terminal previews use a 32,768-character head-and-tail budget and point
to the persisted transcript when truncated. Complete settled results and
transcripts remain available through the read-only agent/history resources in
the [agent manual](../agents.md#agent-resource-results). One agent runs at most
one turn at a time, so transcript order disambiguates results without exposing
opaque turn IDs.

## Rationale and consequences

Explicit follow-up owns child activation, mail owns information, and
[WaitAgent](0036-wait-for-agents-explicitly.md) owns waiting. Separating wake-up
summaries from content avoids duplicate mail in tool output and history.
The idle-root exception lets the coordinator receive late reports without a
user nudge; it reuses the visible Background event path rather than granting
senders a general ability to start or steer idle children.
A stable spawn parent owns automatic results even when another peer starts work;
that peer needs an explicit SendMessage for a direct reply.

Bounded previews avoid copying large outputs into every parent's context.
Interrupted results preserve the invariant that every settled child turn notifies
its parent. Persistence preserves unread information across resume without
turning the model into a delivery-protocol participant.

## Decision history

- **Wake the idle root for agent reports (2026-10).** Previously all SendMessage
  mail and terminal results were queue-only: even reports addressed to `/root`
  could remain stranded until the user's next turn. Agent-originated MAIL and
  ordinary RESULT delivery to `/root` now offer a wake through the existing
  Monitor scheduler. The child activation boundary, active WaitAgent behavior,
  and durable mailbox remain unchanged. Successful workflow consumption needs
  no duplicate wake; failed handlers fall back to ordinary waking mail. Runtime
  marks, non-waking recovery, and no automatic failure/abort retry loop prevent
  persisted or failed work from silently restarting itself.
- **Deliver a working child's completions to the child.** All captured
  completions used to go to the parent at settlement. A September 2026 Goal
  verifier yielded four probe scripts, never polled them, and gave its verdict
  without their results; the root then received all four afterwards as
  unexplained `Bash completed` rows. The completion owner is the child
  (ADR 0024), so it now reads completions while it works, and the parent gets
  only those the child could not read.
- **Separate captured Bash output from terminal results.** Previously the runtime
  concatenated every captured completion onto the final response. A persisted
  reviewer result contained JSON followed by two Bash test logs inside one result
  envelope, so the live view could not display them independently and the settled
  response no longer matched the reviewer's output. Separate existing `EXECUTION`
  records preserve the output and atomic settlement without changing that response.
- **ADR 0035 separated queue-only information from activation.** ADR 0046 chose
  one plain-text message shape instead of speculative structured payloads.
- **ADR 0047 kept acknowledgements minimal** because target/message metadata
  already exists in the call; failures remain visible.
- **ADR 0048 separated mail delivery from wait results**, using one FIFO delivery
  path for waiting, active, and later-resumed recipients. ADR 0049 made unread
  mail durable without adding model-side delivery bookkeeping.
- **ADR 0050 fixed automatic result ownership at the spawn parent**, including
  peer-initiated follow-ups, avoiding shifting ownership and broadcast duplicates.
  It explicitly included interruption because interruption settles a mevedel turn.
- **ADR 0051 chose one RESULT envelope** for completed, errored, and interrupted
  turns. ADR 0052 bounded inline results while reusing persisted transcripts for
  full content. Its original “32 KiB” wording described a character-count limit;
  the implementation uses 32,768 characters, which may occupy more UTF-8 bytes.

These are revisions and facets of one communication contract. The independent
registry, authority, and rendering decisions remain separate ADRs.
