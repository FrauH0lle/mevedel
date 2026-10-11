---
name: "Preserve the requested editing surface"
description: "A whiteboard redesign request means editing that whiteboard, not substituting a separate preview."
type: feedback
---

When asked to redesign selected whiteboard content, implement the change in that whiteboard. Do not substitute a separate HTML preview unless the current request asks for one.

**Why:** A separate artifact does not change the surface the user asked to edit. Preserve the current comment thread's intent rather than carrying over an unrelated earlier artifact request.

**Source:** User correction on 2026-09-29, session `2026-09-29T22-05-32c15165b7f8`, `segment-0001.chat.org`, turn 5: “No, you were supposed to change the whiteboard,” following the separate `data-pipeline-design.html` artifact. The user-provided comment thread in turn 7 reiterated “implement that in the whiteboard.”
