# Return every agent turn result to the spawn parent

Status: accepted

Every settled agent turn automatically enqueues one completion, error, or interruption result to that agent's spawn parent without starting a parent turn. This applies to the initial task and all later follow-ups, regardless of which peer initiated a follow-up. The spawn parent remains the stable result owner; a different initiator that needs a direct reply must receive an explicit `SendMessage`. Results are not broadcast to avoid duplicate delivery and shifting ownership. Unlike current Codex V2, mevedel also reports interruptions: interruption settles a mevedel agent turn, and notifying the parent preserves the invariant that every settled child turn produces one parent result and wakes an explicit wait.
