# Limit active agent turns per session tree

Status: accepted. Incorporates ADR 0055.

## Current decision

One root-session setting bounds active non-root turns across the entire agent
tree, regardless of depth. It defaults to ten and descendants cannot override
it. A turn holds capacity through waiting and human interactions; settlement
releases it. Retained idle identities consume no slot.

Agent reserves capacity before publishing a child. SendMessage reserves a slot
to start an idle non-root target; messaging an active target needs no new slot.
At capacity, mail remains queued and starts the recipient when a slot frees.
Settlement, preparation cancellation, and failed-start rollback reoffer queued
work after releasing capacity.
Idle-root wakes do not consume tree capacity;
see [ADR 0035](0035-separate-agent-messages-from-follow-up-work.md).

## Rationale and consequences

One shared active-turn bound limits concurrent work without confusing runtime
cost with retained identity. Separate depth/total-agent limits, an unlimited
mode, spawn admission queues, and residency eviction are not part of this design.
Waiting still holds a slot, so delegators must account for the tree's capacity.

## Decision history

**October 2026: queue retained-agent work at capacity.** Merging FollowupAgent
into SendMessage removed the idle-follow-up capacity error. Existing mailbox
records retain deferred work; settlement reoffers wakes without a separate
admission queue. ADR 0035 records the stranded-mail evidence for this change.

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
