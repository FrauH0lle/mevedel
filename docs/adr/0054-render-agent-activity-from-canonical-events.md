# Render agent activity from canonical events

Status: accepted

Mevedel renders compact agent activity rows from canonical tool and lifecycle
events: `Started PATH`, `FollowupAgent: PATH`, `SendMessage: PATH`,
`InterruptAgent: PATH`, and `Waiting for agents`. `SendMessage` and
`FollowupAgent` retain distinct labels. Settled `WaitAgent` calls render
`WaitAgent: agents (OUTCOME)`; consecutive visible waits retain the final row
with a count. Canonical events remain unchanged, and UI code does not maintain
an independent activity state that can diverge from agent turns, mailbox
delivery, or tool settlement.
