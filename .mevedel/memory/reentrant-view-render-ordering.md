---
name: "Test both reentry orders and separate mechanism from incident attribution"
description: "Reverse reentrant nesting in regressions; a matching injected failure does not establish the original event sequence"
type: feedback
---

# Test both reentry orders and separate mechanism from incident attribution

When testing code that can reenter through callbacks, fontification, or settlement, exercise both nesting orders. A passing case in one direction does not establish safety in the reverse direction: older work can resume after nested finalization and restore obsolete output or state.

The 2026-09-16 view-render investigation demonstrated this asymmetry:
- Ordinary streaming and a live update nested inside settlement passed.
- Settlement injected during fontification inside an older live update failed: the older update resumed, duplicated response text, and recreated retained-tail state.
- The loaded native functions reproduced the same injected failure in temporary buffers, ruling out a scratch-harness-only explanation.

Compare authoritative data with its rendered projection before attributing duplicated display content to duplicated model output. In this incident, the transcript contained one heading and HTML example, while the view contained multiple copies; fresh rendering was correct.

**Causal limit:** a deterministic injected ordering can prove a defect with matching symptoms without proving the original production event sequence. Historical render tracing was unavailable here, and the full original corruption was not reproduced. Do not present the injected mechanism as a recorded trace of the original incident.

The note's final checkpoint supersedes the earlier memories' “no implementation” status: fixes and rebuilt bytecode/native-runtime checks were completed. That does not resolve the historical causal limit. The original broken view was no longer open at final verification, so its final hash could not be checked.
