# Buddy

Buddy is mevedel's third way of working. The first two are pull: you mark a
region and author a directive, or you type a prompt in the chat buffer. Buddy
is push. A model reads what you just wrote and leaves short notes in the margin
of your source buffer. You did not ask for them, and ignoring one costs a
keystroke.

```
M-x mevedel-buddy-mode        watch this buffer
M-x mevedel-buddy-global-mode watch every buffer in `mevedel-buddy-tracked-modes'
M-x mevedel-buddy-review      review now, without waiting for the idle timer
M-x mevedel-buddy-guide       ask what to build here, rather than what went wrong
M-x mevedel-buddy-dismiss-note   dismiss the note at point
M-x mevedel-buddy-dismiss-notes  dismiss every note in this buffer
M-x mevedel-buddy-clear-changes  forget the edits recorded so far
M-x mevedel-buddy-abort          abandon a review that has wedged
```

## Two channels

Buddy runs one reviewer and one advisor over shared machinery. They differ in
when they fire, what they are sent, and what the prompt permits.

| | automatic review | `mevedel-buddy-guide` |
| --- | --- | --- |
| Trigger | idle timer, unasked | you invoke it |
| Input | consolidated diff of recent edits | region, or whole buffer |
| Uses change tracking | yes | no |
| Requires `mevedel-buddy-mode` | yes | no |
| May speculate | no | yes |
| Default when unsure | stay silent | say something |

They are separate because their noise economics are opposite. Unasked review
must default to silence; comment on everything and the user disables it within
a day. Guidance that defaults to silence is worthless, because speculation is
the product. One always-on prompt cannot be tuned against both targets, so the
automatic channel stays strict and guidance is a command.

Tools, note records, overlays, ids, severity, dismissal, and the request path
are identical between them. Every request carries the whole note set, so a
guidance note raised on a sketch is visible to the review that runs later, once
the code exists, and can be retracted once you have acted on it. Guidance opens
a thread; review closes it. Do not partition notes by originating channel — the
composition is the point.

Guidance preempts an automatic review already in flight, abandoning it without
recording its changes as reviewed. The request you made outranks the one a
timer made.

## Scope

Buddy is workspace-scoped, not buffer-scoped. Changes are keyed by the project
root when the buffer has one, and by buffer name otherwise, so edits spread
across several files of one project are reviewed together. That is what lets
the model see a caller and its callee change in the same round.

Buddy uses no session. It derives its workspace from the source buffer, the way
directive dispatch already does, so it works on a file you have not started a
conversation about. A live session is used only to attribute telemetry when one
happens to exist.

Workspace detection reads `default-directory`, not `buffer-file-name`, so a
brand-new unsaved buffer in a project still gets `AGENTS.md` and persistent
memory in its prompt. That matters most for guidance, where "what should this
module need" depends on project conventions.

The cached scope is bound to the buffer name, visited file, and
`default-directory` that produced it. Changing any of them discards that
buffer's old edit records and resets its pending review timer before deriving
the new scope. A buffer cannot carry one project's edits or prompt context into
another project merely by changing what it visits.

## Everything is ephemeral

Notes, dismissals, recorded edits, note ids, and the last-reviewed time live in
memory and die with the Emacs process. Nothing is written under `.mevedel/`,
nothing enters persistent memory, and nothing lands in a session transcript.
Reopen a file and Buddy re-derives whatever is still true.

**Buddy never writes persistent memory, and that is deliberate.** Its defining
property is that it runs unattended — idle timer, no request, no confirmation.
That is exactly what makes it the wrong producer for durable context: anything
it wrote would be a claim you never reviewed, and `MEMORY.md` feeds straight
into future system prompts. Tutor mode ran this experiment; its hints file
accumulated forever and was read by nobody.

If Buddy annoys you the same way repeatedly, add a line to `AGENTS.md`. That is
a write you made with attention, and it steers everything else too.

Dismissals are also ephemeral. They are described to the model within the Emacs
session, which is enough to stop it repeating rejected advice, and they cost
nothing to lose. It also keeps undo cheap: dismissals are a list, so recovering
an accidental dismissal is a `pop` if that ever becomes worth building.

## Notes are not instructions

Instruction enumeration selects on the `mevedel-instruction` overlay property.
Buddy notes never set it, so they are structurally invisible to instruction
navigation, tinting, persistence, deletion, and subdirective resolution. No
third instruction type exists and no instruction code path knows about notes.
See [ADR 0108](adr/0108-buddy-notes-are-not-instructions.md).

## What a review looks at

A review covers the **region around your change**, not only the lines you
touched. The payload carries six lines of context either side of each change,
and every numbered line in it can carry a note — so a bug sitting next to an
edit gets named even though you did not introduce it this round.

It does not stop there. `read_buffer` returns a bounded range of a buffer
already in the review, for a question the diff raised but cannot settle — a
signature to check, a declaration further up. Both bounds are required and one
call returns at most `mevedel-buddy-note-read-limit` lines. An unbounded read
would ship a whole file to the provider on an idle timer, for a one-line edit,
through a tool that takes no permission step.

Lines read back are annotatable, so a fault the read exposes gets its note where
the fault is rather than described from the diff. That widens where notes may
appear; if it turns noisy, the narrower rule is to gate `add_note` back to diff
lines.

Borderline material is handled by **severity, not silence**. Something a linter
or the byte compiler would also report is not off limits; it is just rarely
worth more than `trivial`. Whether you ever see it is
`mevedel-buddy-severity-floor`'s job, and that is yours to set — at `trivial`
you get cleanups and checkdoc-style remarks along with everything else, which
is a perfectly reasonable way to run it.

## Reviewed edits are retired

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

A note's text is worth more at the end than the beginning — the observation
comes first, the reason it matters comes last — so truncating one loses the half
that justified interrupting you. But laying every note out in full turns a busy
buffer into a wall of blocks.

Buddy takes flycheck's approach and picks the style **per line**, so exactly one
note is ever laid out in full: the one you are reading.

| | default | shows |
| --- | --- | --- |
| `mevedel-buddy-note-current-line-style` | `below` | the whole note, wrapped, indented under the code |
| `mevedel-buddy-note-other-lines-style` | `eol` | one line after the code, shortened to fit |

Either may also be `nil` to hide notes on those lines. Setting
`mevedel-buddy-note-other-lines-style` to `nil` annotates only the line at
point, the way Neovim and Helix show diagnostics.

Layout uses `mevedel-buddy-note-width` — a **fixed** column budget, not the
window width. An overlay's `after-string` is shared by every window showing its
buffer, so a layout fitted to one window would be wrong in another and wrong
again after a split. Truncation is acceptable in `eol` precisely because the
full text is one cursor move away.

Notes are laid out again from `post-command-hook`, which does nothing unless
point actually changed line, and the hook is removed from a buffer once it holds
no notes.

`sideline` (flush right, `lsp-ui-sideline` style) is deliberately not
implemented: it is the one style that needs window geometry, and `below` plus
`eol` already solve the problem.

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

This is the one mevedel workload where a local model is arguably the default
rather than the fallback: it fires constantly on small diffs, latency beats
depth, and your source goes out on every idle timer.

## Divergences from llm-buddy

Buddy is a port of [llm-buddy](https://github.com/ahyatt/llm-buddy) by Andrew
Hyatt. The package itself is not a dependency — it requires the `llm` library
and mevedel is gptel-coupled, so avoiding a second provider abstraction is the
reason this port exists. Four divergences are deliberate:

- **No `end` tool and no forced tool choice.** llm-buddy forces a tool call
  every turn, which means the model can never stop, so it must be handed an
  explicit exit. Buddy inverts it: a turn with no tool call has nothing left to
  say. That drops a tool and removes a dependency on forced tool choice, which
  gptel honors for anthropic, gemini, bedrock, openai, and openai-responses but
  which ollama silently ignores.
- **Tools stay out of the registry and pipeline.** They need argument
  validation but no permission check, no snapshot, and no persistence — one
  pipeline stage out of five. Keeping them unregistered also means nothing
  outside a Buddy request can call them.
- **Scope is the mevedel workspace**, not a `project.el` project, and it is
  derived from `default-directory` rather than requiring a visited file.
- **Reads and notes are bounded.** llm-buddy's `read_buffer` takes optional
  bounds that mean the whole buffer, applies no line cap, and checks no buffer
  allowlist; its `add_note` resolves a line number by counting from `point-min`
  when the tool call arrives. mevedel requires both bounds, caps the span,
  restricts reads to buffers in the running review, and resolves every line
  through markers captured when that line was shown.

llm-buddy's `replace_content` auto-fix path is not ported. mevedel already has
patch proposals, patch review, and the permission chain for edits; a second
route that modifies a buffer unprompted is at odds with that. Notes only.

## Configuration

| Option | Default | Meaning |
| --- | --- | --- |
| `mevedel-buddy-tracked-modes` | `(prog-mode text-mode conf-mode)` | Modes the global mode watches |
| `mevedel-buddy-idle-delay` | `10` | Seconds idle before an automatic review |
| `mevedel-buddy-min-interval` | `60` | Least seconds between automatic reviews of one scope |
| `mevedel-buddy-coalesce-window` | `180.0` | Seconds within which nearby edits merge into one record |
| `mevedel-buddy-severity-floor` | `"significant"` | Lowest severity the model is asked to report |
| `mevedel-buddy-max-iterations` | `8` | Tool rounds before a review is abandoned |
| `mevedel-buddy-timeout` | `120` | Seconds before a review that never settles is abandoned |
| `mevedel-buddy-note-serialize-limit` | `40` | Notes described to the model per request |
| `mevedel-buddy-note-read-limit` | `200` | Lines one `read_buffer` call returns |
| `mevedel-buddy-note-width` | `72` | Column budget for laying a note out |
| `mevedel-buddy-note-current-line-style` | `below` | Style for the line at point |
| `mevedel-buddy-note-other-lines-style` | `eol` | Style for every other line |

mevedel's own surfaces — view, chat, cockpit, inspector — are excluded from
tracking regardless of their major mode.
