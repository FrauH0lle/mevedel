# Persist unread agent mail

Status: accepted

Each agent mailbox is durable root-session state. Saving and resuming a session preserves unread messages and completion notifications, which are injected before the recipient's next model sample. Injection removes a record from the unread FIFO and records it in the retained agent conversation, so content survives thereafter without repeated delivery. Internal persistence may track delivery position, but V2 exposes no model-facing message IDs, acknowledgements, or replay controls.
