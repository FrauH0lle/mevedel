---
name: "Live view reentry during settlement duplicates rendered text"
description: "Settlement injected while a live update is fontifying makes the older update resume, duplicate text, and recreate retained-tail state; natural seam still unproven"
type: project
---

# Live view reentry during settlement duplicates rendered text

Observed 2026-09-16 in the live “Investigate UTF8 Coding Warning” view: the authoritative transcript `.mevedel/sessions/2026-09-16T10-18-06d94b273906/segment-0001.chat.org:2747–2778` contains one “The appropriate fix…” paragraph and one HTML example, while the view buffer contains four and seven. The extra text is real view-buffer content, not repeated model output and not a redisplay artifact.

**What the controlled reproduction isolated** (`.scratch/view-render-investigation-20260916/replay.el:63`, log `artifact://executions/execution-95iYzf.log`):

- Fresh rendering with the loaded renderer was correct, and ordinary streaming with chunk sizes 1, 7, 23, and 51 passed.
- A live update nested inside settlement passed; the inverse ordering failed. Settlement injected while `mevedel-view--fontify-response` is running lets that older live update resume, duplicate text, and recreate retained-tail state: expected `(:tail nil :copies 1)`, observed `(:tail t :copies 2)`. Loaded native functions reproduced the same result in temporary buffers.
- The idle affected view retained `mevedel-view--live-data-tail-start` at 528879 and `mevedel-view--live-view-tail-start` at 3789 even though active-turn markers were cleared; the retained data position points into prose rather than the displayed HTML block.
- Relevant paths: `mevedel-view-render.el:3322–3577`, `mevedel-view-stream.el:1029–1087`.

**Limits — do not overstate.** The injected ordering establishes a defect matching the duplication and stale streaming state, but it does not prove the original session's natural reentry seam and does not reproduce the full corruption. Historical render debugging was disabled. No fix was implemented, and the affected view was not refreshed. The user's instruction for that turn was “Everything is still live. Please investigate first before changing code.” As of capture the broken view and the failing scratch test were still available.
