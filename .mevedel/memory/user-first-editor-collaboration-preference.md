---
name: "User-first, editor-native AI collaboration"
description: "Support user-led, context-isolated collaboration and tangible Emacs prototypes; distinguish user-directed note sending from autonomous initiative."
type: user
---

# User-first, editor-native AI collaboration

User statements recorded 2026-11-08–09:

- The user mostly works agentically, as with Codex or Claude Code, but also wants mevedel to support collaboration where AI is primarily supportive and the user remains in control.
- The design goal is collaboration that “puts the user first.” This supplements agentic work rather than replacing it.
- Because Emacs is an editor, not merely a terminal, the user wants an editor-native gesture: mark text and send it to the LLM with an instruction.
- For marked-code requests, the user preferred isolated requests over adding them to the main chat, because piling unrelated context into one conversation confuses the model.

Additional user statements recorded 2026-11-10 from the notes/Buddy redesign discussion:

- Make design concepts tangible as an **Emacs Lisp prototype**—for example, a mockup buffer, non-functional commands, overlays, comments, and notes—so the user can assess how the interaction feels. A web/HTML sketch alone does not meet that evaluation goal.
- The user liked the child-frame chat window because it fits the “in-buffer workflow,” and liked note actions appearing on key press or when point moves onto them. Typing in the overlay must not accidentally trigger those actions. These are scoped presentation preferences, not a requirement to use child frames for every UI.
- The user wanted **Buddy notes to survive restart**, explaining that otherwise the same request must be repeated after each restart. This concerns the proposed Buddy interaction, not the separate disposable `work://shared/` working-note lifecycle.
- The user distinguished an agent autonomously creating and sending a note, which they found of little value compared with acting in its chat, from a user explicitly asking “Please send this or these notes,” which is a valid use case that should not be prevented.

**How to apply:** evaluate supportive workflows as first-class editor interactions, not merely smaller versions of autonomous agent runs. Preserve relevant request context without assuming every selection-based request belongs in the main conversation. Use editor-native prototypes when assessing the feel of a proposed Emacs interaction. Restrictions on autonomous initiative should not inadvertently prohibit an explicitly user-directed note-sending workflow.

These statements establish goals and preferences, not a settled architecture or implementation status. They do not determine whether Buddy and notes merge, the storage mechanism for restart continuity, or activation behavior. The assistant's “AI may speak unasked, acts only when you ask” rule was not user-confirmed. The activity levels remained tentative; the user explicitly rejected Quiet as the proposed default, so do not present it as their preferred default. The “take a note” idea was interesting to the user, not accepted as a design.
