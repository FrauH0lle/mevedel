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
cursor it held to the inline draft. Setting it with `setq` does the same at
the view's next command.

- **Child frame over the host.** The frame uses the note frame's shared
  child-frame parameters and its active and inactive border faces, repainted
  on focus change. It is inset two columns from each side of the host's text
  area (less on a host too narrow for that), floats half a line above its
  bottom edge and is as tall as the composer from the rail down, capped at
  half the host's text height. The frame is created once per view, hidden
  between uses, and deleted with the view or when the option is turned off.
  Note-frame windows, historical segments and agent transcript views never
  have one.
- **Terminal frames.** In terminal Emacs it is a terminal child frame with
  the same behavior. Its geometry is in whole cells (a terminal's pixel is a
  cell, so the half-line gap rounds to none). Emacs draws a terminal child
  frame's border as box characters one cell outside the frame and only around
  a decorated frame, so the floating composer overrides the shared
  `undecorated` parameter there, uses a one-cell border in the placement and
  places the frame inside it; the box stays inside the host. Emacs 31 draws
  the box in the child frame's own `border` face (bug#80043), so the
  floating composer sets that face's foreground on its frame alone to the
  active or inactive border color, leaving other child frames' borders
  as they are. A shown child's box cells change only when its parent is
  redrawn, so a color change redraws the parent from a timer, outside
  redisplay; an unchanged color redraws nothing. A terminal sends keystrokes to the selected child frame itself and
  `select-frame` never moves a redirect there, so a stale redirect would send
  the host's later keystrokes back to the floating composer: on a terminal
  hand-over selects the floating composer without redirecting its parent.
- **When.** Only the selected window can be a host, and only while it shows
  neither its cursor's draft position nor a full line of the composer (rail
  or draft), so it hides as soon as the host fully shows the first line of a
  composer taller than the window. So it follows
  the selected window across splits, window selection and frame resizes, and
  hides while another frame, tab or buffer is selected; closing its parent
  frame deletes it.
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
  wherever it points by then, unless another view floating over the same
  frame redirected it since: the frame records the last view to redirect it
  (`mevedel-floating-composer-owner`). When the host window is deleted or shows
  another buffer, a window-buffer change hook, installed with the first
  floating composer and removed when the option goes off, clears the
  redirect in that redisplay, before the next keystroke, and a timer then deletes the orphaned
  floating composer. Its border shows the active face while it is the
  selected frame and its parent has compositor focus, repainted on
  compositor focus and window selection changes.
- **Excluded from view geometry.** The floating composer's window carries
  `mevedel-view-geometry-excluded`, so `mevedel-view--windows`' geometry
  answer omits it. Viewport capture and restoration (ADR 0126), composer
  anchoring, blank-space repair and narrowest-window chrome fitting (rail,
  panel rows, mail notices) ignore it, and its own resizes do not refit the
  host's rail. It starts at the rail instead of being anchored; a draft
  taller than the cap scrolls with its cursor, never above the rail.
- **Still counted for visibility.** The rail is on screen in the floating
  composer, so animation visibility keeps counting its window, and its frame
  carries `mevedel-view-animate-as-parent` so animation's frame choice treats
  it as its parent.
- **Slower rail animation in the floating composer.** While every attended
  window showing the request label is a floating composer, the label
  animates at the battery frame-rate ceiling
  (`mevedel-view-spinner-battery-framerate`, the rate the `save` power policy
  always applies), in the Lisp timer and in native presentation alike. The
  Lisp painter repaints the label only once its own period has passed, even
  when a pending-tool row wakes the shared timer sooner. When
  the rail is also visible inline, it runs at the normal rate. Pending-tool
  rows keep their rate. No option controls this: the user accepted the
  floating composer's typing cost and chose to slow, not stop, its animation
  (2026-11-19).
- **Placement cost.** The composer's natural height is measured with a pixel
  limit of the cap plus one line and cached until the buffer
  (`buffer-modified-tick`, so property changes such as a face height count),
  the window width or the face remapping (text scale) changes, so scrolling
  the host measures nothing. The pixel limit holds the parent's line height,
  so global text scaling also invalidates it. Measuring fontifies the draft,
  which itself
  modifies the buffer, so the cache records the buffer's state after
  measuring; keyed before, every key measured twice. Overlay changes do not
  invalidate it. The key stores a copy of the face remapping, which
  `face-remap-add-relative` edits in place. A typed key still measures once before
  redisplay: a PGTK child frame's resize takes effect before the next
  redisplay, so measuring later would show the grown draft scrolled in the
  old height for a frame. Resize and move are one compound call, issued only
  when the requested geometry differs from the frame's own: restoring a
  window configuration, as leaving the minibuffer does, rounds the frame to
  whole lines. Creating the hidden frame and hiding
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
  that is not selected. Settling a card visited with `C-c C-j` is the one
  exception, described below.
- `C-c C-j` from the floating composer moves the cursor to the host first, so
  the visit and the draft offset it remembers belong to the host, and selects
  the card there. While a floating composer is shown over the window, a card
  whose cursor line would be hidden behind it is scrolled to the window's
  top, or, if the card is too tall for that, its cursor line just above the
  floating composer. Arriving cards move nothing.
- Settling the visited card, while the host still visits it, returns the
  cursor to the remembered draft offset: inline when the host fully shows
  that position, otherwise in the shown floating composer, leaving the host's
  point in the transcript.
- Sending with `C-c RET` from the floating composer needs no rule of its own:
  the host's point is in the transcript, so ADR 0126 keeps its viewport as a
  history reader, and the cursor stays in the emptied floating composer.
  Input typed while the submission is prepared is kept as inline.
- With the option off ADR 0126's per-window return-to-draft is unchanged.

### Keys and pointer

Keys act on the selected window; with the cursor in the transcript they keep
their transcript meaning (`n`, `p`, `t`, `TAB`, `RET`).

- `C-c C-SPC` (`mevedel-floating-composer-toggle`) moves the cursor between
  the composer and the transcript: between the floating composer and its
  host while it is shown, else between the inline draft and the transcript,
  each toggle returning to where the previous one left the window's cursor.
  Clicking the floating composer selects it as the toggle does.
- In the floating composer `C-v` and `M-v` page within a capped draft. After
  every command, and whenever redisplay finds it scrolled (touchpad momentum
  scrolls within one command), its window start is clamped to the rail, so neither these
  keys nor the wheel ever show transcript text in it; an uncapped draft
  stays pinned at the rail. Scrolling below the draft's end shows blank
  space, as in any window.
- `C-M-v`/`C-M-S-v` there scroll the host: the view sets
  `other-window-scroll-default` buffer-locally to return the host from the
  floating composer and defer to the global default elsewhere.
- The wheel scrolls the window under the pointer (`mouse-wheel-follow-mouse`)
  and leaves the selection, and so any keystroke redirect, where it was.
- `C-x 2`/`C-x 3` there split the host: the floating composer's window
  carries a `split-window` parameter that splits the host instead. The
  cursor stays in the floating composer.

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

Final run, on the complete feature (integration tree at `74a0a1e9`,
2026-11-19): Emacs 31.1 pgtk, byte-compiled, private presenting KWin
(virtual, OpenGL, no lock screen); presentation probes before and after with
73 and 67 completed frame callbacks; one warm-up round and 6 alternating
rounds; every measured workload began focused and attended except
`idle-unfocused`, which is unattended by design and measured before any
crossing, so `on-never` has never created a frame. The power policy is pinned
to `full`, so only the floating composer's rule applies the battery ceiling
(15 fps); earlier runs left it at `auto`, which in the private session
animated every rail at the ceiling (`off-inline` idle with a request: 86-92 ms
Emacs CPU and 20-21 redisplay cycles per 4 s under `auto`, 142 ms and 36 under
`full`). Crossings count each direction's commands separately; their CPU
includes the benchmark reading per-thread CPU before each command, equally in
every scenario. Values are scenario vs baseline; "paired" is the per-round
difference, median [min, max] over 6 rounds. No parity verdict is recorded
here. A repeat of the whole gate at `34da91d5` measured no cost clearly
above these: medians above a range here were inconclusive (quiet-idle CPU
under 1 ms, stream redisplay cycles with ranges spanning zero), and the
visible floating composer measured less (typing ratio 1.34 [1.26, 1.77],
Emacs CPU +33 [+26, +89] ms; wrapped deletion +29 [+15, +84] ms; stream
against the hidden rail +169 [+167, +226] ms). The second run, before keys,
long drafts, `C-c C-j`, window handling and terminal support landed,
measured the same within these ranges except where noted.

Remaining regressions other than visible rail animation:

- **Typing in the visible floating composer** (`on-visible` vs `off-inline`,
  40 keys per run): latency including redisplay 2.56 vs 1.52 ms median
  (pooled, n 240 each), per-round ratio 1.62 [1.42, 1.74]; Emacs CPU
  +65 [+39, +86] ms per run. A probe in the second run split this: with the
  floating composer's update disabled, keys still cost 1.88-2.40 ms against
  1.36-1.82 ms inline (run medians, 4 alternating runs of 40 keys), so most
  of it is redisplaying the second window; the one remaining measurement
  per key takes about 0.06 ms of layout. The user accepted this cost
  (2026-11-19).
- **Wrapped-draft deletion in the visible floating composer** (30 keys per
  run): Emacs CPU +46 [+34, +55] ms per run and one more GC (+1 [+1, +1]);
  latency is lower, 3.56 vs 4.15 ms median (n 180 each), ratio
  0.87 [0.83, 0.98].
- **Unfocused idle with a floating-composer frame that exists** (4 s, the
  view's frame unfocused): `on-hidden` Emacs CPU +6.5 [-0.4, +18.8] ms and
  redisplay cycles +1 [+0, +8]; `on-visible` +3.5 [-3.9, +16.7] ms and
  +1 [+0, +8]. The CPU differences are inconclusive; the never-created
  control `on-never` also measured +2.6 [-6.3, +14.3] ms with +1 [-1, +7]
  redisplay cycles. A probe found an extra redisplay with no Lisp timer due,
  about 0.9 and 1.4 s after focus left, so the frame's window-system events
  drive it.
- **First show,** which creates the child frame once per view: 50.72 ms
  median, p90 61.85 ms (n 36), no "off" counterpart, with 2 child-frame
  move/resize calls each. Without PGTK's wait this is frame creation itself.

Visible rail animation, with the rail visible only in the floating composer
and so animating at the battery ceiling:

- **Idle with a request, against the hidden rail** (`on-visible` vs
  `off-away`, 4 s): Emacs CPU +88.8 [+80.4, +95.3] ms, KWin CPU
  +27.9 [+19.4, +32.1] ms, redisplay cycles +19 [+18, +19].
- **Idle with a request, against the rail visible inline at the normal rate**
  (`off-inline`): Emacs CPU -47.5 [-57.1, -28.7] ms, KWin CPU
  -19.7 [-33.1, -4.9] ms, redisplay cycles -14 [-14, -13].
- **Stream, 100 chunks at 40 ms, against the hidden rail:** Emacs CPU
  +227.9 [+173.5, +273.1] ms, KWin CPU +28.8 [+6.1, +56.5] ms, redisplay
  cycles +41 [+40, +43], one more GC. Against the inline rail: Emacs CPU
  -15.3 [-75.9, +38.8] ms (inconclusive), KWin CPU -1.0 [-37.4, +34.6] ms,
  redisplay cycles -5 [-9, -4].

No regression measured:

- **Scroll away** (`on-never` vs `off-inline`, 12 crossings per run, n 72
  each): 6.05 vs 7.69 ms median, ratio 0.78 [0.76, 0.83]; Emacs CPU
  -39 [-45, -27] ms and redisplay cycles -1 [-2, -1] per run; KWin CPU
  -0.1 [-1.8, +2.6] ms (the second run's +1.5 ms is gone).
- **Scroll back:** 3.27 vs 5.88 ms median, ratio 0.56 [0.52, 0.58]; p90
  5.53 vs 6.55 ms (the second run's p90 tail, 25.63 vs 12.31 ms, did not
  recur); Emacs CPU +6.6 [-21.7, +18.2] ms (inconclusive; the second run
  measured -42.6), KWin CPU -4.6 [-6.2, -0.1] ms, redisplay cycles
  -4 [-5, -4] per run.
- **Streaming with the floating composer hidden** (`on-hidden`): Emacs CPU
  +25.7 [-44.8, +75.3] ms and KWin CPU +4.9 [-18.7, +25.3] ms per 100 chunks,
  both inconclusive (the second run measured KWin +10.7 [+3.1, +13.6] ms);
  `on-never` +28.6 [-60.7, +82.7] and +3.3 [-13.3, +42.7] ms.
- **On but never created** (`on-never` vs `off-inline`): wrapped deletion
  4.28 vs 4.15 ms median, per-round ratio 1.05 [1.01, 1.08], higher in every
  round of this run only; a repeat of the whole gate at `34da91d5` measured
  ratio 0.94 [0.89, 1.12] (rounds 1-5 quiet, round 6 under an unrelated
  test suite), and isolated per-merge runs and a key-interleaved control
  straddled zero. Typing 1.70 vs 1.52 ms, ratio 1.03 [0.93, 1.17] (repeat:
  0.95 [0.78, 1.17]). Idle with a request +3.6 [-8.5, +23.8] ms; quiet idle
  +0.1 [-0.2, +0.4] ms.
- **Hidden after use** (`on-hidden` vs `off-inline`): typing 1.42 vs
  1.52 ms; wrapped deletion 3.61 vs 4.15 ms; idle with a request
  -0.7 [-7.2, +9.6] ms; quiet idle +0.3 [-0.3, +1.0] ms.
- **Visible floating composer, quiet idle:** Emacs CPU +0.3 [-1.2, +0.7] ms.
- **Child-frame move/resize** calls were 0 in every paired workload (the
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
  helper); the estimate (2026-11-18) was roughly 20 source files and
  2,500-4,000 changed lines plus an ADR 0126 rewrite. Open problems: a new rule for which windows
  follow output (today "caret in the composer"), where the rail and mode
  badge live (ADR 0126 requires real text), `C-c C-j` across buffers, and
  composer placement in the note frame, `/btw` and historical segments.
  Deferred: it is the escalation path if the floating composer measurably
  regresses typing, scrolling or streaming, and a separate architecture item
  regardless.
