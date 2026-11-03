---
name: "Artifact collaboration across chats, machines, and tools"
description: "Preserve viewing-chat context and unified discussions; local Emacs sessions should edit remote artifacts without becoming remote browser-chat participants."
type: user
---

# Artifact collaboration preferences

## Discussion context and presentation

User feedback recorded 2026-10-09:

- When an artifact is attached to a chat and the user comments while viewing it there, use **that chat's context**. Routing every comment to the artifact's dedicated session would lose the context the user explicitly attached.
- Whether `.mevedel/artifacts/` is committed to Git belongs to **the user or project**, not to mevedel.

User statement in the 2026-10-11 session, recorded 2026-10-12:

- Prefer one **Discuss** mechanism for HTML artifacts, whiteboards, and documents rather than separate comment and ask pathways: “It is the right direction.”

User statement recorded 2026-10-14:

- Prefer artifact titles, not hashes, as visible labels in the artifact list: “can we have the artifact titles and not their hashes?”

## Cross-machine and cross-tool collaboration

User statements, 2026-11-02 (`segment-0001.chat.org`, turns 1–2):

- The remote mevedel host is a collaborative space. Whiteboards there feed code written in local sessions; the SNT analysis-pipeline board was the example.
- The user wants local agents to edit shared remote artifacts directly and live, rather than leaving the collaborative board disconnected from local coding work.
- Colleagues often prefer Claude Code. Their AI tools need to be able to update the shared whiteboard too.

**User clarification in turn 9:** the remote-artifact machinery is meant “to allow the editing of a remote artifact via a locally running Emacs + mevedel,” not to make local Emacs participate in remote browser chats about that artifact.

For HTML artifacts, the user meant “just having the possibility to edit the HTML file so it gets updated.” Live HTML co-editing is not part of that stated intent. Do not extrapolate the whiteboard's live-editing goal into an HTML co-editing requirement.

**How to apply:** when evaluating artifact collaboration, account for remote colleagues, local coding sessions, and participants using AI clients other than mevedel. Preserve the distinction between remote artifact editing and remote chat participation. Treat these as user goals, not proof of current capability or approval of a particular protocol.

Detailed routing, discussion-lifecycle, and remote-access designs belong in maintained documentation or the tracker. Keeping leases outside the artifact store was the assistant's implementation inference, not an explicitly confirmed user preference. A headless client, generic MCP wrapper, and particular artifact-link credential model were also assistant proposals, not established user requirements in this evidence.
