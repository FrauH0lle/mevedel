# Deliver agent mail separately from wait results

Status: accepted

`WaitAgent` is a wake-up primitive, not a content transport. It returns only a short completion, steering, or timeout summary. Before the next model sample, the runtime drains queued mailbox entries in FIFO order and injects each as a separate model-visible communication record carrying message type, sender path, recipient path, and payload. The same delivery path applies whether the recipient was waiting, already active, or resumed later, preventing duplicate content in both tool output and conversation history.
