# `utf8` warning investigation — 2026-09-16

Task: user asked whether `Warning: unknown coding system "utf8" [6 times]`
still applies, and pointed to `.mevedel/sessions` for historical evidence.

- Emacs's `sgml-html-meta-auto-coding-function` and XML counterpart emit the
  exact warning for `utf8`; live probes return a UTF-8 coding system without
  warnings for `utf-8` instead.
- Historical evidence: session `test123-2026-09-04T23-43-72c1`, agent transcript
  `agents/explorer--2026-09-05T00-27-54--925edd96.chat.org:145,153,197,867`
  records reads of HTML with `<meta charset=utf8>` from `~/Downloads/claude_design/`.
- Four current files there retain that header on line 1: `mevview-redesign.html`,
  `directives-in-shared-view.html`, `agent-task-panel.html`, `applypatch-review-ui.html`.
- Live diagnostic: the loaded `mevedel-tool-fs-read-slurp-file-contents` from
  Straight's `mevedel-tool-fs-read.elc`, called with offset 1 / limit 1 for each
  file, successfully returned text and emitted the exact warning once per file.
  Diagnostics were captured locally; no advice or persistent runtime changes remain.
- Conclusion: concrete, still-reproducible external-input trigger. Historical
  session records support attribution but do not preserve the exact original
  six-message burst. Correct the source headers to `charset="utf-8"` if desired;
  no Mevedel workaround is established as necessary. No source or input files changed.
