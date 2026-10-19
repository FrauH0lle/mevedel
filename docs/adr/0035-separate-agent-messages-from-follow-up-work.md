# Wake retained agents through messages

Status: accepted. Incorporates ADRs 0046, 0047, 0048, 0049, 0050, 0051, and 0052.

## Current decision

SendMessage is the single communication tool for retained agents. It delivers
nonempty plain text to an active recipient at its next safe boundary and starts
an idle recipient's turn. Agent-originated MAIL to `/root` uses the Monitor
Background event scheduler. Self-mail never starts a turn. The same text shape
applies to Agent's initial task. Shared files can carry larger referenced
material; there is no structured attachment or dual text/item protocol.

Successful Agent returns the canonical child path. SendMessage has an empty
successful model result when queued for an active recipient or started at once;
at capacity it queues the message and reports that the agent starts when a slot
frees. The original call retains target and text. Invalid targets and
dispatch/delivery failures remain tool errors. Observation/control tools retain
their own result shapes.

Unread mail is durable root-session state and is delivered FIFO before the
recipient's next sample, independently of WaitAgent. A communication record
carries type, sender/recipient paths, and payload. Once delivered it lives in
conversation history rather than the unread queue. There are no model-facing
message IDs, replay controls, or acknowledgements.

Every settled child turn stages one terminal RESULT for its original spawn
parent, including later turns initiated by a peer. An ordinary RESULT wakes an
idle spawn parent, root or non-root, but does not broadcast.
Outcomes are completed, errored, or interrupted; payloads
carry the final response, concise failure, or interruption reason with useful
partial text. A workflow awaiting its child may consume that exact committed
record through its result handler without waking the parent. Handler failure
leaves it for ordinary waking mail.
A yielded Bash completion that arrives while the child still works reaches the
child itself, as `EXECUTION` mail read at its next provider request, like the
root's own completions. Completions the child can no longer read, because they
arrive after its final answer or remain unread at settlement, are committed
alongside that result as separate `EXECUTION` records to the spawn parent, with
the child as sender. They do not alter the final-response payload or the
workflow result handler's input.

Idle-root wakes share Monitor's scheduling order and holds. Non-root wakes are
offered once a settlement ends, oldest waking mail first while capacity
allows. Releasing a preparation reservation or rolling back a failed startup
also reoffers waiting work. This includes mail arriving after the recipient's
last sample and queued work waiting for a slot. No retained agent in a session
starts while one of its settlements is running: settlement marks its agent idle
before the RESULT commits, so a new turn could race its rollback or publication.
Mail meanwhile waits for the offer that follows every settlement, whether it
committed, rolled back, or failed. A delivery retry that runs after the agent's
next turn started leaves that turn's buffer state alone. An active WaitAgent
resumes through its existing callback rather than starting another turn.

Wakes use current session permissions and sticky Plan state, not authority from
mail. MAIL sent under Plan additionally retains a runtime read-only stamp for
any non-root activation that will read it, including a later direct assignment.
RESULT records are information and do not carry the sender's Plan stamp.
Wake and Plan marks are runtime-only: restart and
recovery do not turn saved mail or recovery-generated results into background
work. An interrupt request clears existing wake marks on the target, including
an idle target waiting for capacity; later MAIL or RESULT can wake it. A child
result arriving after its parent's interruption therefore restarts that parent.
Errored settlement
clears its marks to prevent automatic retries.
Successful root turns likewise reoffer late mail; failed or aborted root turns
do not automatically retry it. EXECUTION records never trigger agent wakes.

Inline terminal previews use a 32,768-character head-and-tail budget and point
to the persisted transcript when truncated. Complete settled results and
transcripts remain available through the read-only agent/history resources in
the [agent manual](../agents.md#agent-resource-results). One agent runs at most
one turn at a time, so transcript order disambiguates results without exposing
opaque turn IDs.

## Rationale and consequences

The harness owns delivery and activation; callers need not distinguish an idle
recipient from one about to finish its final response.
[WaitAgent](0036-wait-for-agents-explicitly.md) owns waiting within a turn.
Separating wake-up summaries from content avoids duplicate mail in tool output
and history. Root notification reuses the Background event wake path (its prompt
and heading are hidden in the view; the turn starts with report notices).
Every SendMessage holder can wake retained agents, including communicating leaf
roles. Capacity bounds concurrent turns; there is no separate message-loop guard.
A stable spawn parent owns automatic results even when another peer starts work;
that peer needs an explicit SendMessage for a direct reply.

Bounded previews avoid copying large outputs into every parent's context.
Interrupted results preserve the invariant that every settled child turn notifies
its parent. Persistence preserves unread information across resume without
turning the model into a delivery-protocol participant.

## Decision history

- **Merge follow-up work into SendMessage (2026-10-19).** Inspection found 44
  unread MAIL/RESULT records at idle agents across 30 sessions. Ten MAIL records
  arrived during final responses, and the late-mail race reproduced during the
  investigation itself. Separating information from activation required callers
  to predict recipient state and stranded nested results. The user chose Claude
  Code's idle-recipient wake behavior: remove FollowupAgent, wake for MAIL and
  ordinary RESULT, and retain queued work until capacity frees. Runtime Plan
  stamps preserve read-only limits when activation is deferred; interruption
  clears old wake marks without discarding mail or blocking a later assignment.
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
