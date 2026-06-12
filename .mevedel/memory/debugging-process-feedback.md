---
name: debugging process feedback
description: User preference for isolating recurring errors before implementation changes.
type: feedback
---

Bisect recurring test/log emitters before changing implementation.

**Why:** Jumping to implementation changes can introduce unnecessary code when the real source is a narrower test or cleanup path.
**How to apply:** For repeated suite/log errors, first isolate the responsible command, test file, test group, or buffer lifecycle path; only then make the smallest change justified by that isolation.
