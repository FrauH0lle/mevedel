# Centralize agent interactions in the root session

Status: accepted

User-facing `Ask` interactions from any agent enter the root session's existing interaction queue and display the requesting canonical path. The answer is delivered only to the requesting turn, which remains active and consumes capacity while waiting. Interrupting the turn removes its unanswered prompts. Agent transcript buffers remain inspection surfaces and do not host independent interactive prompt UIs.
