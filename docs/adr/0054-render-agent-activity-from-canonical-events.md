# Render agent activity from canonical events

Status: accepted

Mevedel renders compact agent activity rows from canonical tool and lifecycle events: `Started PATH`, `Interacted with PATH`, `Interrupted PATH`, `Waiting for agents`, and `Finished waiting`. `SendMessage` and `FollowupAgent` share the interaction label while their underlying tool calls retain the semantic distinction. UI code does not maintain an independent activity state that can diverge from agent turns, mailbox delivery, or tool settlement.
