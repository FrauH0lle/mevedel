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
  limit of the cap plus one line and cached until the buffer, the window
  width or the face remapping (text scale) changes, so scrolling the host
  measures nothing. Resize and move are one compound call, issued only when
  the requested geometry changed. Updates run after the user's commands in
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

### Performance gate against "off" (2026-11-19): not parity

Measured with the floating composer performance gate (see the development
guide) on the integration tree at `b9cb7956`: Emacs 31.1 pgtk, byte-compiled,
private presenting KWin (virtual, OpenGL, no lock screen); presentation probes
before and after with 72 and 70 completed frame callbacks; one warm-up round
and 6 alternating rounds. Values are scenario vs baseline; "paired" is the
per-round difference, median [min, max] over 6 rounds. The verdict is **not
parity**, and it is not only visible rail animation. Tickets 05-09 stop and
the separate composer buffer is reopened (not assumed to pass).

Regressions other than visible animation:

- **Focus is lost after the cursor returns inline.** The scroll-back hand-over
  calls `select-frame-set-input-focus` on the host's frame after the floating
  composer held focus. Under KWin, that frame then never reports focus again.
  The view counts as unattended, so stream renders are deferred and the rail
  stops animating until focus returns by other means. A plain `emacs -Q`
  child frame reproduces the focus loss without mevedel; it was not checked
  on the desktop compositor. It invalidates every `on-hidden` measurement
  (the view was unattended in all of them) and means the crossings below
  started from an unfocused view. The graphical ERT suite stubs
  `frame-focus-state`, so it cannot see this.
- **Typing in the visible floating composer** (`on-visible` vs `off-inline`,
  40 keys per run): latency including redisplay 2.59 vs 1.76 ms median
  (pooled, n 240 each), per-round ratio 1.48 [1.42, 1.55]. Emacs CPU
  +50 [+41, +55] ms per run, about +1.2 ms per key.
- **Scroll back from the floating composer** (`on-never` crossings vs
  `off-inline`): 105.45 vs 5.15 ms median (n 72 each), per-round ratio
  20.35 [20.23, 20.84]. The constant +100 ms is not the focus call itself,
  which returns at once; its cause is not established.
- **Scroll away into the floating composer:** 8.26 vs 6.96 ms median (n 72
  each), per-round ratio 1.20 [1.13, 1.21]. Per 12 crossings: KWin CPU
  +59 [+53, +62] ms and redisplay cycles +22 [+21, +23].
- **First show,** which creates the child frame once per view: 157 ms
  median, p90 166 ms (n 36). There is no "off" counterpart.
- **Unfocused idle with a floating-composer frame present** (`on-never` vs
  `off-inline`, 4 s): Emacs CPU +28 [+11, +38] ms. `on-visible` measured
  +3 [-30, +16] ms, which is inconclusive.

Visible rail animation, measured against the equal-visible-work control:

- **Idle with a request in flight, 4 s:** `on-visible` vs `off-away` adds
  Emacs CPU +105 [+99, +106] ms, KWin CPU +29 [+27, +32] ms and 18
  redisplay cycles. Against the control (`off-inline`, rail visible inline)
  the differences are -2 [-7, +3] ms and +1 [-3, +3] ms: the cost is the
  animation the floating composer makes visible.
- **Stream, 100 chunks at 40 ms:** `on-visible` vs `off-away` adds Emacs CPU
  +178 [+114, +213] ms, KWin CPU +37 [+9, +78] ms and redisplay cycles
  +36 [+34, +42]. Against the control: Emacs CPU +17 [-56, +46] ms and KWin
  CPU +24 [-6, +54] ms (both inconclusive), redisplay cycles +15 [+9, +17].

Parity:

- **On but never shown** (`on-never` vs `off-inline`): typing 1.73 vs
  1.76 ms, ratio 1.00 [0.73, 1.02]; wrapped-draft deletion 4.30 vs 4.27 ms,
  ratio 1.01 [0.98, 1.05]. Stream Emacs CPU +11 [-44, +25] ms with
  redisplay cycles +0 [-7, +3]. Idle with a request: Emacs CPU
  -1 [-5, +4] ms. Quiet idle: +1 [-1, +7] ms.
- **Visible floating composer, quiet idle and wrapped deletion:** quiet idle
  Emacs CPU +1 [-5, +3] ms; deletion 3.78 vs 4.27 ms, ratio
  0.89 [0.87, 0.90].
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
