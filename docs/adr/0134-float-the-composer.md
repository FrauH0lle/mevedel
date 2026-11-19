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
  compositor focus); clicking into the floating composer sets it. Its border
  shows the active face while it is the selected frame and its parent has
  compositor focus.
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
  limit of the cap plus one line and cached until the buffer's text, the
  window width or the face remapping (text scale) changes, so scrolling the
  host measures nothing. Text property changes alone keep the cached height,
  as overlay changes do; the view's own property updates after each key
  otherwise measured every key twice. A typed key still measures once before
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
- **Per-keystroke height measurement:** a property-only change after each
  key missed the cache, so every key measured twice.
- **Idle CPU with a floating-composer frame:** the benchmark scanned every
  process inside its measured interval, summed per-thread CPU so an exiting
  thread made it smaller, and its "unfocused" frame showed the view itself.
  The benchmark now measures per-thread deltas outside the scan, and the
  unfocused frame shows another buffer. The earlier idle numbers are not
  comparable with those below.

Second run, on the tree that records this section: Emacs 31.1 pgtk,
byte-compiled, private presenting KWin (virtual, OpenGL, no lock screen);
presentation probes before and after with 72 and 65 completed frame
callbacks; one warm-up round and 6 alternating rounds; every measured
workload began focused and attended except `idle-unfocused`, which is
unattended by design. Values are scenario vs baseline; "paired" is the
per-round difference, median [min, max] over 6 rounds. No parity verdict is
recorded here.

Remaining regressions other than visible rail animation:

- **Typing in the visible floating composer** (`on-visible` vs `off-inline`,
  40 keys per run): latency including redisplay 2.20 vs 1.45 ms median
  (pooled, n 240 each), per-round ratio 1.54 [1.42, 1.67]; Emacs CPU
  +39 [+32, +56] ms per run. A probe split this: with the floating
  composer's update disabled, keys still cost 1.88-2.40 ms against
  1.36-1.82 ms inline (run medians, 4 alternating runs of 40 keys), so most
  of it is redisplaying the second window; the one remaining measurement
  per key takes about 0.06 ms of layout.
- **Wrapped-draft deletion in the visible floating composer** (30 keys per
  run): Emacs CPU +27 [+1, +49] ms per run and up to one more GC; latency is
  lower, 3.41 vs 4.17 ms median (n 180 each), ratio 0.81 [0.76, 0.87].
- **Unfocused idle with a floating-composer frame** (4 s, the view's frame
  unfocused): `on-never` Emacs CPU +5.6 [+3.4, +10.5] ms, KWin CPU
  +3.1 [-0.4, +4.3] ms; `on-hidden` +1.6 [-1.8, +3.9] ms and `on-visible`
  +1.5 [-2.3, +7.4] ms (both inconclusive). Each has +2 [+2, +2] redisplay
  cycles. A probe found one extra redisplay with no Lisp timer due, about
  0.9 and 1.4 s after focus left, so the frame's window-system events, not
  mevedel code, drive it.
- **Idle with a request, against the equal-visible-work control**
  (`on-visible` vs `off-inline`, 4 s): redisplay cycles +1.5 [+1, +2];
  Emacs CPU +3.6 [-3.5, +18.5] ms and KWin CPU +0.5 [-0.2, +3.8] ms are
  inconclusive.
- **Streaming against the same control** (100 chunks at 40 ms): redisplay
  cycles +16.5 [+16, +19]; Emacs CPU -27.4 [-61.7, +54.8] ms and KWin CPU
  -1.4 [-14.3, +27.3] ms are inconclusive.
- **Scroll away into the floating composer:** one more GC per 12 crossings
  (2 vs 1); latency is lower (below).
- **First show,** which creates the child frame once per view: 44.54 ms
  median, p90 58.93 ms (n 36), no "off" counterpart. Without PGTK's wait
  this is frame creation itself.

Visible rail animation (`on-visible` vs `off-away`, the rail hidden):

- **Idle with a request in flight, 4 s:** Emacs CPU +84.1 [+82.4, +91.2] ms,
  KWin CPU +23.2 [+21.4, +25.1] ms, redisplay cycles +20 [+20, +20].
- **Stream, 100 chunks at 40 ms:** Emacs CPU +196.3 [+117.8, +211.4] ms,
  KWin CPU +23.0 [-3.5, +42.5] ms, redisplay cycles +40 [+40, +41], one more
  GC.

No regression measured:

- **Crossings** (`on-never` vs `off-inline`, n 72 each): scroll back 2.98 vs
  5.74 ms median, ratio 0.54 [0.47, 0.55]; scroll away 5.79 vs 7.52 ms,
  ratio 0.80 [0.72, 0.86], KWin CPU +1.5 [-1.1, +1.9] ms per 12 crossings.
- **On but never shown and hidden after use** (`on-never`, `on-hidden` vs
  `off-inline`): typing 1.46 and 1.26 vs 1.45 ms; wrapped deletion 4.01 and
  3.33 vs 4.17 ms; stream Emacs CPU -27.1 [-63.9, +26.0] and
  -23.6 [-53.0, +21.4] ms; idle with a request +1.8 [-2.6, +5.6] and
  +0.6 [-9.0, +9.0] ms; quiet idle -2.7 and -2.7 ms.
- **Visible floating composer, quiet idle:** Emacs CPU -2.6 [-4.7, -2.3] ms.
- **Child-frame move/resize** calls were 0 in every measured workload (the
  draft height did not change); show/hide were 2 per crossing.

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
