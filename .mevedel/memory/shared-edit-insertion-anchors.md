---
name: "SharedEdit insertion anchors can be rejected as stale"
description: "A SharedEdit document patch that anchors to blocks inserted by the same patch can fail with Stale insertion anchor; retrying without afterId worked"
type: project
---

# SharedEdit insertion anchors can be rejected as stale

Observed 2026-09-19 (journal digest `46801e8d`, session `2026-09-19T12-48`): a `SharedEdit` document patch that chained insertions through `:afterId` anchors — including blocks the same patch had just inserted — was rejected with `Error: Stale insertion anchor`. A `SharedRead` taken immediately after showed the item still at revision 1, so the whole transaction had been rejected. Re-issuing the insertions without `:afterId` succeeded at revision 2, with the sections returned in the intended order.

- Treat this as one observation, not a proven mechanism. The record shows the anchored patch failing and the unanchored retry succeeding; it does not establish that the same-patch anchors were the sole cause of the rejection.
- Practical sequence when a multi-block document patch is rejected as stale: re-read the item for the current revision and block IDs, then retry a simpler patch (no `afterId` chain pointing at blocks that the same patch creates) instead of resubmitting the identical patch.
- Read `docs/shared-editing.md` for the documented contract (a stale target rejects the whole transaction and returns current target data); use this note for the observed failure and the retry that worked.
