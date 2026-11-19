# Float the composer as a second window on the view buffer

Status: accepted. Amends the scroll-away rules of
[ADR 0126](0126-keep-the-composer-still.md) as described under "Cursor and
scrolling".

## Decision

While the selected view window is scrolled away from the composer, a
floating composer shows the same composer in a child frame over the bottom
of that window, the host. It is a second window on the view buffer, not a
copy and not a separate buffer: draft, rail, mode badge, keymaps and live
progress are the view buffer's own. `mevedel-floating-composer` (default on)
turns it off; turning it off removes every floating composer and returns a
cursor it held to the inline draft.

- **Child frame over the host.** The frame uses the note frame's shared
  child-frame parameters and its active and inactive border faces, repainted
  on focus change. It is inset two columns from each side of the host's text
  area (less on a host too narrow for that), floats half a line above its
  bottom edge and is as tall as the composer from the rail down, capped at
  half the host's text height. The frame is created once per view, hidden
  between uses, and deleted with the view or when the option is turned off.
  For now only graphical frames have one; note-frame windows, historical
  segments and agent transcript views never do.
- **When.** Only the selected window can be a host, and only while it shows
  neither its cursor's draft position nor the whole composer.
- **Focus.** The floating composer never takes compositor focus
  (`no-accept-focus`). Under PGTK a child frame that once held compositor
  focus never returns it to its parent (`select-frame-set-input-focus` on the
  parent does not, only deleting the child does), which left the view
  unattended: stream renders deferred and the rail stopped. Hand-over
  therefore selects the floating composer's window and redirects the host
  frame's keystrokes to it with `redirect-frame-focus`; hand-back clears the
  redirect and selects the host. Selecting another window from the floating
  composer, for example by clicking the host, retargets the redirect at the
  newly selected frame (`select-frame` does this for the frame holding
  compositor focus); clicking into the floating composer sets it. Removing
  the floating composer (view killed, option off) clears the redirect it set,
  wherever it points by then. When the host window is deleted or shows
  another buffer, a window-buffer change hook clears the redirect in that
  redisplay, before the next keystroke, and a timer then deletes the orphaned
  floating composer. Its border shows the active face while it is the
  selected frame and its parent has compositor focus, repainted on
  compositor focus and window selection changes.
- **Excluded from view geometry.** The floating composer's window carries
  `mevedel-view-geometry-excluded`, so `mevedel-view--windows`' geometry
  answer omits it. Viewport capture and restoration (ADR 0126), composer
  anchoring, blank-space repair and narrowest-window chrome fitting (rail,
  panel rows, mail notices) ignore it, and its own resizes do not refit the
  host's rail. It starts at the rail instead of being anchored; a draft
  taller than the cap scrolls with its cursor.
- **Still counted for visibility.** The rail is on screen in the floating
  composer, so animation visibility keeps counting its window, and its frame
  carries `mevedel-view-animate-as-parent` so animation's frame choice treats
  it as its parent.
- **Placement cost.** The composer's natural height is measured with a pixel
  limit of the cap plus one line and cached until the buffer
  (`buffer-modified-tick`, so property changes such as a face height count),
  the window width or the face remapping (text scale) changes, so scrolling
  the host measures nothing. Measuring fontifies the draft, which itself
  modifies the buffer, so the cache records the buffer's state after
  measuring; keyed before, every key measured twice. Overlay changes do not
  invalidate it. A typed key still measures once before
  redisplay: a PGTK child frame's resize takes effect before the next
  redisplay, so measuring later would show the grown draft scrolled in the
  old height for a frame. Resize and move are one compound call, issued only
  when the requested geometry changed. Creating the hidden frame and hiding
  it bind `pgtk-wait-for-event-timeout` to nil: PGTK otherwise runs its event
  loop for that whole timeout (0.1 s) on every hide. Updates run after the user's commands in
  the view and, coalesced into one timer, on the view's render cadence and
  window size, buffer and selection changes; the timer is not armed while
  the floating composer is hidden and the selected window's cursor is in the
  draft.

### Cursor and scrolling (amends ADR 0126)

- When the user's scroll command in the selected window pushes its draft
  cursor off screen, the floating composer takes the cursor at the same
  draft offset. The offset is ADR 0126's pending return, captured before
  native scrolling displaced point; handing over consumes it, so a later
  inline return cannot overwrite edits made in the floating composer. The
  host's own point stays in the transcript: it remains a history reader.
- A cursor in the transcript stays there; the floating composer is shown
  without taking it.
- A scroll command after which the floating composer's cursor position is
  fully visible in the host hides the floating composer and returns the
  cursor there, so a draft taller than the window can return too.
- Only `post-command-hook` moves the cursor. Timers, window hooks, streaming
  and other background updates only place, show or hide a floating composer
  that is not selected.
- With the option off ADR 0126's per-window return-to-draft is unchanged.

## Rationale and consequences

Problem: while writing a prompt the user often needs to look something up in
the transcript, and had to scroll back and forth.

Redisplay cost (from the Emacs C source, 30.2 and 32.0.50 trees; measured
by the performance gate below): a buffer change flags every window showing
the buffer for its next redisplay, where each is updated incrementally;
windows showing only other
buffers are normally skipped. Several changes before one redisplay coalesce.
The floating composer therefore adds an incremental update of its window to
redisplays that follow typing or streaming, only while it is shown. Without
the geometry exclusion it would also add a full anchoring pass per stream
update (about 1 ms each in ADR 0126's measurements) and narrow the
transcript's chrome by its inset.

The rail is fitted to the host, so in the narrower floating composer a long
rail wraps; the measured height includes the wrap, and since the floating
composer's width follows only the host there is no resize loop.

### Performance gate against "off"

The floating composer performance gate (see the development guide) first ran
on the integration tree at `b9cb7956` (2026-11-19) and did not reach parity.
Its findings had these causes, now removed:

- **Focus loss after the cursor returned inline,** which left the view
  unattended (renders deferred, rail stopped): PGTK child-frame focus, see
  "Focus" above. The graphical ERT suite stubbed `frame-focus-state` and could
  not see it; it now runs with real compositor focus.
- **A constant +100 ms on scrolling back** (105 vs 5 ms): PGTK's
  `make-frame-invisible` always runs GTK's event loop for the whole
  `pgtk-wait-for-event-timeout`. Creating the hidden frame paid the same wait,
  100 of the 157 ms first show.
- **Per-keystroke height measurement:** the measurement's own fontification
  modified the buffer after the cache key was taken, so the next update
  measured again: every key measured twice.
- **Idle CPU with a floating-composer frame:** the benchmark scanned every
  process inside its measured interval, summed per-thread CPU so an exiting
  thread made it smaller, and its "unfocused" frame showed the view itself.
  The benchmark now measures per-thread deltas outside the scan, and the
  unfocused frame shows another buffer. The earlier idle numbers are not
  comparable with those below.

Second run, on the tree that records this section: Emacs 31.1 pgtk,
byte-compiled, private presenting KWin (virtual, OpenGL, no lock screen);
presentation probes before and after with 70 and 66 completed frame
callbacks; one warm-up round and 6 alternating rounds; every measured
workload began focused and attended except `idle-unfocused`, which is
unattended by design and measured before any crossing, so `on-never` has
never created a frame. Crossings count each direction's commands
separately; their CPU includes the benchmark reading per-thread CPU before
each command, equally in every scenario. Values are scenario vs baseline;
"paired" is the per-round difference, median [min, max] over 6 rounds. No
parity verdict is recorded here.

Remaining regressions other than visible rail animation:

- **Typing in the visible floating composer** (`on-visible` vs `off-inline`,
  40 keys per run): latency including redisplay 2.63 vs 1.75 ms median
  (pooled, n 240 each), per-round ratio 1.46 [1.34, 1.69]; Emacs CPU
  +56 [+30, +73] ms per run. A probe split this: with the floating
  composer's update disabled, keys still cost 1.88-2.40 ms against
  1.36-1.82 ms inline (run medians, 4 alternating runs of 40 keys), so most
  of it is redisplaying the second window; the one remaining measurement
  per key takes about 0.06 ms of layout.
- **Wrapped-draft deletion in the visible floating composer** (30 keys per
  run): Emacs CPU +41 [+19, +51] ms per run and up to one more GC
  (+1 [+0, +1]); latency is lower, 3.81 vs 4.31 ms median (n 180 each),
  ratio 0.86 [0.77, 0.95].
- **Unfocused idle with a floating-composer frame that exists** (4 s, the
  view's frame unfocused): `on-hidden` Emacs CPU +2.0 [-2.2, +7.6] ms and
  redisplay cycles +0.5 [+0, +1]; `on-visible` Emacs CPU +3.1 [-1.0, +18.9] ms
  and redisplay cycles +1 [+0, +8]. The CPU differences are inconclusive. A
  probe found an extra redisplay with no Lisp timer due, about 0.9 and 1.4 s
  after focus left, so the frame's window-system events drive it.
- **Idle with a request, against the equal-visible-work control**
  (`on-visible` vs `off-inline`, 4 s): Emacs CPU +5.6 [-0.2, +8.8] ms; KWin
  CPU -0.5 [-5.5, +1.4] ms and redisplay cycles +0 [-1, +1].
- **Streaming against the same control** (100 chunks at 40 ms): redisplay
  cycles +13 [+7, +19]; Emacs CPU +5.5 [-10.7, +41.0] ms and KWin CPU
  +7.8 [-62.7, +27.1] ms are inconclusive.
- **First show,** which creates the child frame once per view: 53.12 ms
  median, p90 58.46 ms (n 36), no "off" counterpart. Without PGTK's wait
  this is frame creation itself.

Visible rail animation (`on-visible` vs `off-away`, the rail hidden):

- **Idle with a request in flight, 4 s:** Emacs CPU +90.4 [+77.5, +93.7] ms,
  KWin CPU +27.0 [+21.1, +30.5] ms, redisplay cycles +19 [+18, +19].
- **Stream, 100 chunks at 40 ms:** Emacs CPU +194.3 [+169.8, +236.1] ms,
  KWin CPU +32.9 [-48.0, +52.3] ms, redisplay cycles +39.5 [+35, +41], one
  more GC.

No regression measured:

- **Scroll away** (`on-never` vs `off-inline`, 12 crossings per run, n 72
  each): 6.09 vs 7.69 ms median, ratio 0.80 [0.78, 0.81]; Emacs CPU
  -41 [-67, -20] ms, KWin CPU -1.1 [-1.9, +0.2] ms, redisplay cycles
  -1 [-1, -1] and GCs -1 [-1, +0] per run.
- **Scroll back:** 3.30 vs 6.05 ms median, ratio 0.55 [0.53, 0.56]; Emacs CPU
  -1.8 [-27.8, +1.0] ms, KWin CPU -4.8 [-7.6, -3.8] ms, redisplay cycles
  -4 [-4, -4] per run; GCs +0.5 [-1, +1] (inconclusive).
- **On but never created** (`on-never` vs `off-inline`): typing 1.71 vs
  1.75 ms; wrapped deletion 4.24 vs 4.31 ms; stream Emacs CPU
  -1.1 [-7.5, +18.0] ms; idle with a request +0.8 [-2.3, +4.2] ms; quiet idle
  +0.2 [-0.1, +0.5] ms; unfocused idle +0.7 [-5.0, +6.7] ms with redisplay
  cycles +0 [-1, +1].
- **Hidden after use** (`on-hidden` vs `off-inline`): typing 1.49 vs 1.75 ms;
  wrapped deletion 3.68 vs 4.31 ms; stream Emacs CPU +14.3 [-14.0, +46.3] ms;
  idle with a request +1.3 [-4.9, +6.5] ms; quiet idle +0.6 [+0.2, +0.9] ms.
- **Visible floating composer, quiet idle:** Emacs CPU -0.0 [-0.3, +0.6] ms.
- **Child-frame move/resize** calls were 0 in every measured workload (the
  draft height did not change); show/hide were 1 per crossing direction.

## Alternatives

- **Docked window below the transcript.** Covers nothing, but shrinks the
  transcript window whenever it appears, reflowing the transcript and firing
  size hooks across the view; it costs the same extra window redisplay.
  Rejected; the child frame is the user's chosen presentation.
- **Inline composer with a jump key.** No extra cost, but the user cannot see
  the draft and the passage being looked up together. Rejected.
- **Separate composer buffer.** The only variant expected to reduce
  redisplay work: typing would no longer flag the transcript window and
  streaming would no longer flag the composer window (other redisplay
  triggers remain). About half of ADR 0126 exists only because draft and
  streamed text share a buffer (composer anchoring, draft blank-space repair,
  return-to-draft on scroll, composer-only growth repair, draft guards) and
  would go away; reader and visited-row anchors, last-page fill, vscroll
  preservation and panel stability remain. The composer is built around
  `mevedel-view--input-marker`; composer-related symbols have about 270
  source and 2,200 test text matches (unaudited, tests mostly through one
  helper); the estimate was roughly 20 source files and 2,500-4,000 changed
  lines plus an ADR 0126 rewrite. Open problems: a new rule for which windows
  follow output (today "caret in the composer"), where the rail and mode
  badge live (ADR 0126 requires real text), `C-c C-j` across buffers, and
  composer placement in the note frame, `/btw` and historical segments.
  Deferred: it is the escalation path if the floating composer measurably
  regresses typing, scrolling or streaming, and a separate architecture item
  regardless.
