---
name: view composer preservation feedback
description: Regression-test active composer drafts when changing mevedel view redraw paths.
type: feedback
---

Mevedel view/UI redraw work must regression-test active composer draft preservation, especially async status/agent/task redraws while the user is typing.

**Why:** Async status updates have previously inserted rendered status text into the editable composer, especially with multiline drafts that begin with a literal `>`.
**How to apply:** When touching `mevedel-view` or status-zone materialization paths, add or update tests that redraw while the composer contains a multiline draft starting with `>`, and assert the draft text and point are unchanged.
