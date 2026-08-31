---
name: artifact
description: Build a self-contained HTML mockup, prototype, or document as a session artifact the host and collaboration guests can open
argument-hint: "[what to build]"
context: inline
user-invocable: true
allowed-tools:
  - Eval
---

$ARGUMENTS

# Session artifacts

An artifact is a file the user can open and look at: an HTML mockup, an
interactive prototype, a Markdown document, a diagram image. The artifacts
cockpit lists files in the session's artifacts directory. A settled ApplyPatch
that creates or updates one of those files also publishes its openable card to
a live collaboration room.

This session's artifacts directory:

```!el
(if-let* ((session (or (bound-and-true-p mevedel--session)
                       (and (bound-and-true-p mevedel--data-buffer)
                            (buffer-live-p mevedel--data-buffer)
                            (buffer-local-value 'mevedel--session
                                                mevedel--data-buffer))))
          (save-path (mevedel-session-save-path session)))
    (mevedel-session-artifacts-artifacts-dir save-path)
  "unavailable: no live session; tell the user instead of guessing a path")
```

## Rules

- Create or update one artifact file with ApplyPatch, using an absolute path
  inside the directory above. A successful reviewed patch is the publication
  event; overwriting the same artifact replaces its card.
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
- Name the file for what it shows (`checkout-flow-mockup.html`, not
  `test.html`); the name is the label on every card and cockpit row.
  Subdirectories are allowed but rarely worth it.
- Scratch files, test pages, and intermediate output belong elsewhere. Files
  in this directory appear in the host cockpit; a successful ApplyPatch is
  what additionally publishes a collaboration card.
