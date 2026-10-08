# Native animation and Emacs Canvas investigation

Investigated 2026-10-08 against Emacs master `6d14004c581982231bd959f586c5c835ef347256`
and minad/emacs-canvas-patch `0a614cdc8df1d30442615c0ef4ffc7d5148ea983`.
The original note below was source inspection only. The subsequent
[renderer experiments](renderer-lab/README.md) measured Canvas and implemented
a mevedel-side Wayland presenter; see that report for the current result.

**Recommendation:** investigate PGTK presentation first. Canvas is a useful
pixel-update API and is already in Emacs 32 development sources, but its
current implementation still reaches the same PGTK whole-widget presentation
path. A Rust/C rewrite of already-precomputed spinner frames does not address
that path. This is a source-based prediction; a controlled experiment should
verify the size of any canvas improvement.

## What Canvas actually provides

Emacs 32's NEWS lists canvas support. The patch repository now carries only
the Emacs 31 backport; its instructions require applying the patch and
rebuilding Emacs. This is confirmed in actual upstream code, not just the
project README. [Emacs NEWS](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/etc/NEWS#L512-L520),
[patch README](https://github.com/minad/emacs-canvas-patch/blob/0a614cdc8df1d30442615c0ef4ffc7d5148ea983/README.org).

Canvas stores mutable ARGB32 pixels. On Cairo builds it reuses the existing
image surface, copies the updated pixels, and marks that surface dirty.
`canvas-refresh` redraws matching image glyphs into the frame's backing
buffer. It deliberately does not flush or flip the frame: ordinary redisplay
after a timer/command performs that step. The refresh API has no dirty-region
argument. [image.c, canvas_prepare_for_display and Fcanvas_refresh](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/src/image.c#L5742-L5909).

The glyph fast path avoids relaying out surrounding text, although it scans
current window glyph matrices for matching images; when frame/window state
is inconsistent it defers to normal redisplay. This can benefit changing
images, plots, video, and complex drawing. It is not an independent compositor
surface. [xdisp.c, redraw_image_glyphs](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/src/xdisp.c#L32862-L32936).

## Why the expensive presentation remains

Current Emacs master still implements `pgtk_frame_up_to_date` by flipping the
Cairo context and calling `gtk_widget_queue_draw` whenever buffer flipping is
not blocked. `pgtk_handle_draw` paints the backing surface with `cairo_paint`.
The canvas backport changes no PGTK source.
[pgtkterm.c, frame update](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/src/pgtkterm.c#L3454-L3465),
[draw callback](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/src/pgtkterm.c#L5095-L5119),
[backport patch](https://github.com/minad/emacs-canvas-patch/blob/0a614cdc8df1d30442615c0ef4ffc7d5148ea983/canvas-31.patch).

GTK explicitly defines `queue_draw` as invalidating the entire widget.
GTK also supports area invalidation; exploiting it correctly requires Emacs
to track what changed, preserve backing-buffer correctness, and avoid an
additional whole-widget invalidation. A smaller canvas alone cannot change
this. [GTK queue_draw](https://docs.gtk.org/gtk3/method.Widget.queue_draw.html),
[GTK queue_draw_area](https://docs.gtk.org/gtk3/method.Widget.queue_draw_area.html).

This agrees with the investigation's separately measured large/small-frame
cost difference for a timer that performs no work. It predicts that merely
changing the spinner's rendering language will retain much of the cost.

## Native modules and threads

The current module extension is **only `env->canvas_data`**. There are no
current module methods named `canvas_redraw` or `canvas_refresh`; a module
invokes Lisp `canvas-refresh` through `env->funcall`. The writable pointer
remains valid while the canvas is alive and its dimensions stay fixed.
[module-env-32.h](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/src/module-env-32.h),
[Module Canvas API](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/doc/lispref/internals.texi#L2022-L2068).

A module can perform expensive independent computation on its own threads,
but it cannot call Emacs APIs from arbitrary worker threads: those calls
require an active Emacs call stack and an Emacs-created Lisp interpreter
thread. A worker is therefore no supported escape hatch for independently
refreshing the editor. The canvas API exposes pixels, not GTK widgets,
GPU surfaces, or a custom presentation hook. [module calling restrictions](https://github.com/emacs-mirror/emacs/blob/6d14004c581982231bd959f586c5c835ef347256/doc/lispref/internals.texi#L1362-L1376).

## Most informative next experiment

Use disposable build directories and a separate editor process, without
installing over the user's Emacs. Compare the same frame size, scale, display
visibility, timer cadence, and simple animation under: baseline Emacs 31;
the same source plus canvas; and a separately identified PGTK patch that
skips unchanged presentations and/or propagates accurate dirty regions.
Measure editor and compositor CPU for idle, a no-op timer, and an actual tiny
animation at 8/12/30 Hz. Check resize, scrolling, overlapping windows, cursor,
and expose behavior before considering a presentation patch usable. Canvas
can initially use precomputed Lisp pixel buffers; a native module is justified
only if profiling then finds significant pixel-generation cost.


## Subsequent measurement

The later [renderer experiments](renderer-lab/README.md) built the referenced
Emacs master and measured native Canvas drawing: roughly 55% editor CPU at
30 Hz, comparable to ordinary text on this PGTK setup. They also found a
separate native Wayland subsurface can bypass that presentation path at about
2% CPU. The earlier recommendation to investigate presentation is supported;
the earlier discussion of native rendering should not be read as excluding a
module which owns a separate compositor surface. Product integration is still
under investigation.
