---
name: "Judge inline image previews by displayed size"
description: "Compact previews are a visual requirement, not a preference for pixel or fractional settings"
type: feedback
---

When adjusting Mevedel's inline image previews, judge the actual displayed size rather than whether the setting uses pixels or a window-width fraction. Preserve click-to-enlarge behavior.

**Source:** On 2026-10-09, the user corrected the proposed sizing approach (`segment-0001.chat.org`, completed turn 4) and confirmed the revision in completed turn 5. Relative to the screenshot supplied in that conversation, previews needed to be at least 50% smaller; a proposed reduction from 600 to 400 was insufficient.

**Why:** Changing the configuration representation or making a modest numerical reduction does not necessarily solve the visual footprint the user objected to.

**How to apply:** Evaluate rendered previews in their display context when changing image sizing. Treat the 50% comparison as specific to that supplied screenshot, not a rule to halve every future default or every image dimension.
