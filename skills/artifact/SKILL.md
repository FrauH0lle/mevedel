---
name: artifact
description: Build a self-contained HTML mockup, prototype, or document as a project artifact that others can open; whiteboards and co-edited documents are shared items
argument-hint: "[what to build]"
context: inline
user-invocable: true
allowed-tools:
  - Eval
---

$ARGUMENTS

# Project artifacts

An artifact is a file the user can open and look at: an HTML mockup, an
interactive prototype, a Markdown document, a diagram image. Artifacts live
in the project's artifact store, one directory per artifact; the directory
name is the artifact's id. Any session of the project can open and edit
them, and each turn that writes the artifact file records a version.

This project's artifact store:

```!el
(if-let* ((session (or (bound-and-true-p mevedel--session)
                       (and (bound-and-true-p mevedel--data-buffer)
                            (buffer-live-p mevedel--data-buffer)
                            (buffer-local-value 'mevedel--session
                                                mevedel--data-buffer)))))
    (mevedel-artifact-store-directory (mevedel-session-workspace session))
  "unavailable: no live session; tell the user instead of guessing a path")
```

## Rules

- Start a new artifact with ApplyPatch: Add File at
  `<store>/<id>/<name>`, where `<id>` is a new directory named for what the
  artifact shows (`checkout-flow/`, not `test/`; letters, digits, `-` and
  `_`) and `<name>` its file (`index.html`, `notes.md`). An existing
  directory means the id is taken, even by another file name: list the
  store first, then pick another id, or update that artifact if it is the
  one you meant.
- Update an existing artifact with ApplyPatch on its file. Another session
  may have changed it since you read it; a failed hunk means reread and
  patch again. Writing an artifact attaches it to this session, and a
  successful write also publishes its card to a live collaboration room.
- The first file written into an id directory is the artifact; only it is
  versioned.
- **Self-contained, always.** No CDN scripts or stylesheets, no external
  fonts, no runtime `fetch`, no remote images. In the browser the
  artifact renders inside a sandbox whose Content-Security-Policy blocks
  every network request, so an external reference does not degrade — it
  silently breaks. Inline all CSS and JavaScript; embed images as
  `data:` URIs.
- **Keep it small.** The whole file is sent when a guest opens it, phones
  included, and files over 16 MB are refused outright. Inlined
  images are the usual culprit: keep them few, small, and compressed.
  These are two faces of one rule — self-contained is what makes files
  large, so budget for it.
- HTML renders sandboxed (scripts run, network does not), Markdown and
  images render in the viewer, plain text shows as text; any other type
  is offered to the guest as a download.
- **Embed data safely.** Strict JSON is not enough inside an HTML
  `<script type="application/json">` block: the HTML parser still recognizes
  `</script>` and comment-like text. Serialize the data, then replace every
  literal `<` with `\u003c` before embedding it. Parse with `JSON.parse` and
  emit labels with `textContent`, never `innerHTML`. This applies to every
  data block, including charts and tables.
- **Be honest about controls.** A control must perform its advertised local
  action or be visibly identified as part of a static prototype. Do not imply
  saving, filtering, approval, or host integration that is not implemented.
  Explain unavailable behavior in visible text, not only in a tooltip.
- Scratch files, test pages, and intermediate output belong elsewhere.
  Everything in the store is listed for the whole project.
