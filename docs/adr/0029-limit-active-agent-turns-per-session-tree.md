# Limit active agent turns per session tree

Status: accepted. Incorporates ADR 0055.

## Current decision

One root-session setting bounds active non-root turns across the entire agent
tree, regardless of depth. It defaults to ten and descendants cannot override
it. A turn holds capacity through waiting and human interactions; settlement
releases it. Retained idle identities consume no slot.

Agent reserves capacity before publishing a child. FollowupAgent reserves a slot
only to start an idle target; steering an already active target needs no new
slot. An idle follow-up at capacity fails immediately without queueing its task.
SendMessage remains available for queue-only communication.

## Rationale and consequences

One shared active-turn bound limits concurrent work without confusing runtime
cost with retained identity. Separate depth/total-agent limits, an unlimited
mode, admission queues, and residency eviction are not part of this design.
Waiting still holds a slot, so delegators must account for the tree's capacity.

## Decision history

**October 2026: raise the default from three to ten non-root turns.** The
user requested more room for parallel work and nested delegation. Inspection
confirmed that admission and persistence already support arbitrary positive
capacities; the previous value had no documented technical constraint or
measurement behind it. The root is excluded, allowing eleven active agents
in total. Existing sessions retain their persisted capacity.

**ADR 0029 established the tree-wide bound; ADR 0055 clarified idle follow-up
admission.** Treating every follow-up as a new turn would reject useful steering
when the tree was full. Active steering therefore keeps its existing slot;
idle activation uses the same capacity boundary as a new spawn. These records
express one admission decision and are consolidated here.
