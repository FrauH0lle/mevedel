# Centralize agent interactions in the root session

Status: accepted

User-facing `Ask` interactions from any agent enter the root session's existing interaction queue and display the requesting canonical path. The answer is delivered only to the requesting turn, which remains active and consumes capacity while waiting. Interrupting the turn removes its unanswered prompts. Agent transcript buffers remain inspection surfaces and do not host independent interactive prompt UIs.

Permission decisions follow the same ownership contract. Native ACP children
admit an ordinary request in their conversation buffer, so they use the existing
request cancellers and permission-queue sweep. Their invocation owns native
history and terminal publication; a gptel state machine is unnecessary.
The MCP pipeline validates that the invocation still owns its admitted request.
Neither finishing nor interrupting a child advances the root turn count.
Invocation ownership alone left an interrupted child's permission card queued
after terminal publication; reusing ordinary request admission keeps the
existing cleanup contract without another interaction registry.
