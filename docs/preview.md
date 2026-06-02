# Inline diff preview

Entry point: `mevedel-preview-mode-add-preview` (keyword API). Dispatches
on `mevedel-preview-mode--effective-mode`:

- `default` / `plan` → interactive inline overlay
- `accept-edits` / `trust-all` → `--auto-apply` (runs `apply-fn`
  immediately, still produces a persistent diff summary in the view)

`mevedel-preview-mode` is a buffer-local minor mode with a lighter
` Preview[N]`. Prefix `C-c p`: `n`/`p` navigate, `a` approve-all,
`r` reject-all. Per-overlay: approve (`C-c C-c`/`a`/`RET`), reject
(`C-c C-k`/`r`/`q`), ediff (`C-c C-e`/`e`), feedback (`C-c C-f`/`f`),
trust-rest (`S`), toggle (`TAB`).

`S` approves all pending overlays and escalates permission mode to
`accept-edits` (not `trust-all` — shell commands still prompt). Registering
a preview adds a canceller to the active request's `cancellers` list, so
`mevedel-abort` tears everything down cleanly.

## Handler return shape

Callbacks fire with plist:

```
(:result STR :render-data (:kind diff :patch PATCH :path PATH :rel-path REL))
```

The pipeline splits `:result` (LLM-facing) from `:render-data`
(LLM-invisible side channel). Plain strings still work for legacy
handlers.

## Overlay-preserving apply

`mevedel-diff-apply.el` applies accepted diffs while keeping instruction
overlays usable. It records affected overlays before changing text,
applies all hunks atomically, then moves each overlay once after all
cumulative deltas are known.

A single instruction overlay can be touched by multiple hunks. Keep the
original overlay bounds with each saved hunk record and merge duplicate
records before moving the overlay; otherwise later hunks can compute from
temporary properties already cleared by an earlier move. Line-based
overlays snap back to full lines, and deleted overlays become stubs so
instruction persistence still has a live anchor.

Persist moved instruction state only after `save-buffer` succeeds. If
saving fails, do not write the moved overlay state; otherwise persisted
instructions can point at content that never reached disk.
