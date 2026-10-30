---
name: "Keep display snapshots isolated from mutable canonical data"
description: "When exposing display-only snapshots to observers or handlers, copy mutable leaves so consumers cannot change canonical state."
type: feedback
---

When exposing display-only snapshots to observers or handlers, ensure mutable leaves are copied as well as container structure. Shallow copies can leave strings or nested values aliased, allowing a consumer that mutates its snapshot to change canonical evidence or handler input.

Apply this when designing or reviewing snapshot and observer boundaries. Test by mutating returned nested values and checking both the handler's inputs and a fresh canonical snapshot remain unchanged. Do not broaden the snapshot's authority or infer that arbitrary model text can execute host code.
