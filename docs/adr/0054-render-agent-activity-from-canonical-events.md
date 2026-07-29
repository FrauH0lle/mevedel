# Render agent activity from canonical events

Status: accepted

Mevedel renders compact agent activity rows from canonical tool and lifecycle
events: `Started PATH`, `Interacted with PATH`, `Message sent to PATH`,
`Interrupted PATH`, and `Waiting for agents`. `SendMessage` and
`FollowupAgent` retain distinct labels, while settled `WaitAgent` calls add no
redundant completion row. UI code does not maintain an independent activity
state that can diverge from agent turns, mailbox delivery, or tool settlement.
