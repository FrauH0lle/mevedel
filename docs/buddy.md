# Buddy

Buddy reviews recent edits and adds short advisory notes beside source code.
Enable it per buffer or globally, request an immediate review, or use the
guidance command for suggestions about a region. Remarks are workspace notes
with Buddy as author, using the same labels and modified-key actions as your
notes. They survive restarts and never edit your code.

```
M-x mevedel-buddy-mode        watch this buffer
M-x mevedel-buddy-global-mode watch every buffer in `mevedel-buddy-tracked-modes'
M-x mevedel-buddy-set-level   set this workspace's Quiet, Watching or Active dial
M-x mevedel-buddy-review      review now, without waiting for the idle timer
M-x mevedel-buddy-guide       ask what to build here, rather than what went wrong
M-x mevedel-buddy-dismiss-note   dismiss the note at point
M-x mevedel-buddy-dismiss-notes  dismiss every Buddy note in this workspace
M-x mevedel-buddy-keep-note      keep the remark as your own note
M-x mevedel-buddy-clear-changes  forget the edits recorded so far
M-x mevedel-buddy-abort          abandon a review that has wedged
/buddy [BRIEF|off]               from a session: implement it yourself, with Buddy
```

## Workspace dial and taken notes

`mevedel-buddy-set-level` saves one level per workspace in `.mevedel/buddy-level`
and enables tracking in its source buffers. The default is **Active**:

- **Quiet:** no unasked remarks, except on taken-note code.
- **Watching:** likely mistakes around recent edits, at your pause; no ideas.
- **Active:** Watching plus ideas near a known goal: a taken note or an authored
  note near the edit. Elsewhere it behaves like Watching.

The dial governs automatic reviews only. Explicit `mevedel-buddy-review` and
`mevedel-buddy-guide` requests bypass it and its idea-nearness restriction.
The saved value is read once, at the workspace's first review, not while
recording keystrokes or restoring source notes. An absent setting means Active;
an invalid or unreadable setting reports a diagnostic and also uses Active.

`mevedel-note-take` turns on Buddy for that source buffer and invites review of
the note's code with its text as the brief, at every dial level. Use **take** in
the note's modified-key menu or `T` in the notes list. The label says **Buddy
watches**. Buddy may mark it **looks done** when its stated goal is met, but
never closes it: `mevedel-note-done` is explicit. Editing the goal or its code
retires earlier completion feedback. Automatic taken-only reviews do not resend
unchanged code and goals after a successful review. Changes made in flight,
an abandoned review, a new brief or dial level, or taking again still invite a
review. Explicit review commands are never suppressed.

**Drop** (`mevedel-note-drop`, or `T` again in the list) returns the note to
unsent, retaining its conversation and source code. Sending a taken note makes
it sent. Dropping, sending or closing ends that note's invitation; ordinary
dial-based reviews may still run. After a restart, restored taken notes remain
taken but show **paused**. Restoration neither starts Buddy nor reads the saved
dial. Taking the note again resumes its watch; setting the workspace dial resumes
its takes, including files visited later, for this Emacs session. There is no
prompt on visit. Unknown remote files remain
unclaimed on visit; explicit Buddy activation uses normal workspace detection.

## Feeding a session to Buddy

`/buddy` in a session hands what the session knows about the work to Buddy, so
you can implement it yourself while Buddy reviews and guides with that intent
in view. It takes the first available brief:

1. the text after the command, as in `/buddy add retry with backoff`;
2. in Plan mode, the current plan unless it was already accepted;
3. the objective of an unfinished Goal;
4. the latest request and the assistant's response to it.

The brief is stored for the session's project scope, the same key source
buffers in that project use, and prefixed to every automatic review and
guidance request in that scope. It is capped at 12,000 characters because each
review pays for it again. `/buddy` then turns on `mevedel-buddy-global-mode`.
A later `/buddy` replaces the brief. `/buddy off` forgets it and turns the
global mode off. A session outside a project has no scope to share and is
refused.

The command changes nothing else. Plan mode, a pending proposal and its
approval card, the Goal, the transcript, and the selected window all stay as
they were. You can still accept the plan later, or keep asking the planning
conversation questions while you write the code.

The brief is model input, not an instruction: the review prompt treats code
that contradicts it as a finding and unfinished work as no finding, and the
guidance prompt orients suggestions toward it. It uses the Buddy transport and
model unchanged; no session request is made. Collaboration guests cannot
invoke `/buddy`, because it switches on Buddy across the host's Emacs.

## Two channels

Buddy runs one reviewer and one advisor over shared machinery. They differ in
when they fire, what they are sent, and what the prompt permits.

| | automatic review | `mevedel-buddy-guide` |
| --- | --- | --- |
| Trigger | idle timer, unasked | you invoke it |
| Input | consolidated diff of recent edits | region, or whole buffer |
| Uses change tracking | yes | no |
| Requires `mevedel-buddy-mode` | yes | no |
| May suggest ideas | Active, near a known goal | yes |
| Default when unsure | stay silent | say something |

They are separate because their noise economics are opposite. Unasked review
must default to silence; comment on everything and the user disables it within
a day. Guidance that defaults to silence is worthless, because speculation is
the product. One always-on prompt cannot be tuned against both targets, so the
automatic channel stays strict and guidance is a command.

Tools, note records, overlays, ids, severity, dismissal, and the request path
are identical between them. Every request carries a bounded set of active and dismissed notes from its scope, so a
guidance note raised on a sketch is visible to the review that runs later, once
the code exists, and can be retracted once you have acted on it. Guidance opens
a thread; review closes it. Do not partition notes by originating channel — the
composition is the point.

Guidance cancels its source buffer's queued automatic review and preempts one
already in flight, abandoning it without recording its changes as reviewed.
The request you made outranks the one a timer made.

When the `buddy` workload resolves to Claude Code, Buddy starts an isolated ACP
conversation and exposes only its captured note tools over a private MCP
connection. The same note handlers enforce buffer scope and review ownership.
The native post-tool-batch hook checks the iteration limit before further model
work. Abandonment preserves unreviewed changes and closes the tool scope.

## Scope

Buddy is workspace-scoped, not buffer-scoped. Changes are keyed by the pinned
workspace root when present, otherwise by project root or buffer name, so edits
spread across several files of one workspace are reviewed together. That is what lets
the model see a caller and its callee change in the same round.

Buddy uses no session. It derives its workspace from the source buffer,
so it works on a file you have not started a
conversation about. A live session is used only to attribute telemetry when one
happens to exist.

Without a pinned workspace, detection reads `default-directory`, not
`buffer-file-name`, so a brand-new unsaved buffer in a project still gets
`AGENTS.md` and persistent
memory in its prompt. That matters most for guidance, where "what should this
module need" depends on project conventions.

The cached scope is bound to the buffer name, visited file, and
`default-directory` and pinned workspace root that produced it. Changing any of
them discards that buffer's old edit records and resets its pending review timer before deriving
the new scope. A buffer cannot carry one project's edits or prompt context into
another project merely by changing what it visits.

## Durable remarks, transient reviews

Remarks persist in the workspace instruction store with their note identity,
source anchor, author, text and severity. A new review restores existing remarks
before building its bounded note context, so a restart does not erase earlier
advice. An exact repeated live remark on the same line reuses its identity;
dismissed remarks match by buffer and text even after lines move. Dismissed
remarks take priority in the bounded prompt so open and kept notes cannot
crowd out the advice already rejected.
The first file-backed remark in a recognized project creates `.mevedel/`, like
an ordinary note. Outside a project, an existing workspace state directory is
needed for persistence; otherwise remarks stay in memory with a one-time notice.
Remarks in buffers without a visited file stay in memory only; guidance still
works there, but there is no durable file anchor to save or restore.
Buddy can update or retract a remark when a later review finds its concern
changed or fixed. Retraction removes the remark outright, not source code; it
does not add a recently closed entry. Buddy may raise the concern again if it
returns later.

Dismissal moves a remark to the ordinary recently closed list with reason
`dismissed`. Single dismissal and dismiss-all affect only Buddy-authored notes;
your own notes stay. Dismissed advice remains in bounded review context while
the closed record survives the ordinary count/age retention limits. It is not
a permanent mute rule. Keep changes the author to you, retaining the note's
identity, text and kept-from-Buddy metadata. Buddy cannot subsequently update
or retract a kept note, including from a review already in flight.
Sending or taking a Buddy remark first performs that same ownership transfer,
so it cannot be retracted after acquiring a conversation or becoming taken.

All remark mutations in one review are synced together once at settlement,
including successful tool calls before an abandoned review. The normal checked
store writer handles locking, merging and retryable failures. There is no
per-remark write or new debounce timer. Explicit user actions use the ordinary
note dirty-store flush.

Recorded edits, request markers, briefs and the last-reviewed time remain
transient. Buddy does not write curated memory or insert remarks into a session
transcript. It can record diagnostic lifecycle events through an available
session's telemetry writer.

To make durable guidance, edit the applicable `AGENTS.md` or explicitly request
a memory update through the ordinary workflow. Buddy's passive request has no
memory-saving policy.

Buddy uses the ordinary note record and presentation rather than a separate
annotation system. See [ADR 0108](adr/0108-buddy-remarks-are-workspace-notes.md)
for the change from ephemeral annotations to workspace notes.

## What a review looks at

A review covers the **region around your change**, not only the lines you
touched. The payload carries six lines of context either side of each change,
and every numbered line in it can carry a note — so a bug sitting next to an
edit gets named even though you did not introduce it this round.
Quiet limits new or updated remarks to taken-note code; Watching refuses ideas; Active
permits ideas only within six lines of a captured authored goal whose text
still matches. Taken goals are included even without a pending edit, and
completion feedback is accepted only while both the goal and captured code
still match the live taken note. Each taken section sends at most
`mevedel-buddy-note-read-limit` lines around the edit or source cursor, with
`read_buffer` available for the rest. Paused takes near an Active edit supply
their goal as ordinary nearby notes, not as watched code.

It does not stop there. `read_buffer` returns a bounded range of a buffer
already in the review, for a question the diff raised but cannot settle — a
signature to check, a declaration further up. Both bounds are required and one
call returns at most `mevedel-buddy-note-read-limit` lines. An unbounded read
would ship a whole file to the provider on an idle timer, for a one-line edit,
through a tool that takes no permission step.

Lines returned by read_buffer are annotatable, so notes can address evidence
beyond the original diff while remaining within the captured buffer scope.

Findings need concrete evidence and enough value to justify an inline note;
no notes is a valid result. Severity describes the actual impact, including
findings a linter or compiler could also report. Whether you ever see it is
`mevedel-buddy-severity-floor`'s job, and that is yours to set — at `trivial`
you get cleanups and checkdoc-style remarks along with everything else, which
is a perfectly reasonable way to run it.

## Reviewed edits are retired

Review preparation runs in scheduled steps, assembling one source buffer's diff
per callback before sending the request. The one-review slot covers preparation
as well as the provider request. Editing, renaming, killing, or untracking a
source during preparation abandons that snapshot without retiring pending edits.
Guidance and abort can cancel preparation through the same review ownership.
Each individual buffer diff still runs synchronously; an unusually large file
can exceed the normal callback budget.

A settled review discards the changes it covered, so the next one sends only
what you have written since. Edits made *while* a request was in flight survive
it, and an abandoned or timed-out review retires nothing, so its edits are
offered again rather than silently skipped.

A review that never settles is abandoned after `mevedel-buddy-timeout`, which
releases its markers and frees the one-at-a-time slot. `mevedel-buddy-abort`
does the same by hand. Abandoning also retires the review's generation, so a
callback racing with cancellation settles into nothing rather than retiring
changes it never got notes for. Each review uses its own hidden live request
buffer as the exact `gptel-abort` identity, so cancellation cannot select an
ordinary request from the source buffer and still works after that source dies.

Turning `mevedel-buddy-mode` off in a buffer, or disabling
`mevedel-buddy-global-mode`, **discards that buffer's recorded edits** along
with the tracking. Pending unreviewed feedback is lost, deliberately: without
the tracking hooks nothing would drop those records later, and a buffer that
reused the name — reopened, or uniquified once a second file of that name is
visited — would have their offsets replayed against unrelated content.

## Scope is an allowlist, and empty means nothing

While a review runs, `mevedel-buddy-note--scope-buffers` maps each allowed name
to the exact live buffer the review may touch, and every tool — `read_buffer`,
`add_note`, `update_note`, `remove_note` — checks it. The note set described to
the model is filtered the same way, so a review of one project never sees
another's buffer names, line numbers, or note text.

Names are only model-facing addresses. Killing an allowed buffer and creating
another under the same name does not transfer the old review's authority to
the replacement.

An empty scope denies everything. That matters because a tool call can still
arrive after a review is abandoned or times out, and "no review is running"
must not read as "every buffer in Emacs".

## How a note is laid out

The common note label sits below the complete source line and shows its number,
Buddy authorship, bounded text and available actions. The full authored text is
retained for editing. The modified-key menu offers edit, dismiss and keep;
plain letters still insert source text. Moving point never switches between
two Buddy-specific render styles, and no Buddy relayout hook scans remarks on
each line move.

## Line numbers resolve through markers

The model answers with the line numbers it was shown, but you keep typing while
the request is in flight. Buddy captures markers before sending and resolves
the model's line numbers against them, so a note lands on the text it was
written about even when lines were inserted above it meanwhile. Markers move
with buffer edits; raw line numbers do not.

Only the lines the model was actually shown are marked. Emacs walks a buffer's
marker list on every insertion, so marking every line of a large file would
make typing lag for as long as the request runs.

A new note requires the live captured boundaries for that shown line. Unshown,
deleted, nonpositive, out-of-range, and already released line numbers are
rejected rather than falling back to current raw line counting. A note on the
wrong line is worse than no note, so this is not an optional refinement.

`read_buffer` registers markers for the lines it returns, which is what makes
them annotatable. A line already marked keeps its original markers: recapturing
one would move an existing note's anchor to wherever that number points now,
which is the failure markers exist to prevent.

After the initial payload markers are captured, reads do not grow the set past
`mevedel-buddy-note--marker-ceiling`. A review may issue several reads per round
and several rounds, and without a ceiling the marker set would grow with what
the model asked for rather than with what the user edited. A read stops before
the first line whose marker would exceed the ceiling, so every line it returns
remains annotatable. Initial diff and guidance markers are not truncated; they
are the authority the request started with rather than model-requested growth.

If editing moves an older marker so that its numeric key now names different
text, a later read stops before reusing that ambiguous number. The next review
supplies fresh line numbers. Preserving the older marker keeps a note about the
original diff line correct; returning new text under the same number would not.

## Model selection

Buddy resolves the `buddy` entry in `mevedel-model-workloads`, defaulting to the
fast tier. Retier it, pin an exact provider, or override it per preset through
the usual `:model-workloads` key:

```elisp
(setf (alist-get 'buddy mevedel-model-workloads)
      '(:provider "Ollama:qwen2.5-coder"))
```

The selected provider receives the captured source evidence when Buddy runs.
Automatic requests follow the idle and minimum-interval settings below. A
pause that comes too soon after the scope's last automatic review is not
dropped: the review waits until the interval has passed, unless a further edit
restarts the idle delay first.

## Request boundary

Buddy creates request-local tools for bounded reads and note maintenance. They
are not registered as general tools and do not use the ordinary mutation pipeline.
A response without another tool call ends the review; there is no end tool or
forced tool choice. Buddy has no auto-fix tool: code edits use the normal patch
and permission workflow. The port's rationale is recorded in
[ADR 0108](adr/0108-buddy-remarks-are-workspace-notes.md#decision-history).

## Configuration

| Option | Default | Meaning |
| --- | --- | --- |
| `mevedel-buddy-tracked-modes` | `(prog-mode text-mode conf-mode)` | Modes the global mode watches |
| `mevedel-buddy-idle-delay` | `10` | Seconds idle before an automatic review |
| `mevedel-buddy-min-interval` | `60` | Least seconds between automatic reviews of one scope; edits settling sooner are reviewed once it passes |
| `mevedel-buddy-coalesce-window` | `180.0` | Seconds within which nearby edits merge into one record |
| `mevedel-buddy-severity-floor` | `"significant"` | Lowest severity the model is asked to report |
| `mevedel-buddy-max-iterations` | `8` | Tool rounds before a review is abandoned |
| `mevedel-buddy-timeout` | `120` | Seconds before a review that never settles is abandoned |
| `mevedel-buddy-note-serialize-limit` | `40` | Notes described to the model per request |
| `mevedel-buddy-note-read-limit` | `200` | Lines one `read_buffer` call returns |

mevedel's own surfaces — view, chat, cockpit, inspector — are excluded from
tracking regardless of their major mode.
