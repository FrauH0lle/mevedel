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
