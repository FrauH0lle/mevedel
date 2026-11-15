---
name: "User-first, editor-native AI collaboration"
description: "Support user-led, context-isolated collaboration, editor-native prototypes, and lightweight note interactions without redundant commands."
type: user
---

# User-first, editor-native AI collaboration

## User-led collaboration and context isolation

User statements recorded 2026-11-08–09:

- The user mostly works agentically, as with Codex or Claude Code, but also wants mevedel to support collaboration where AI is primarily supportive and the user remains in control.
- The design goal is collaboration that “puts the user first.” This supplements agentic work rather than replacing it.
- Because Emacs is an editor, not merely a terminal, the user wants an editor-native gesture: mark text and send it to the LLM with an instruction.
- For marked-code requests, the user preferred isolated requests over adding them to the main chat, because piling unrelated context into one conversation confuses the model.

Additional user statements recorded 2026-11-10:

- The user wanted Buddy notes to survive restart, explaining that otherwise the same request must be repeated after each restart. This concerns the Buddy interaction, not the separate disposable `work://shared/` working-note lifecycle.
- The user distinguished an agent autonomously creating and sending a note, which they found of little value compared with acting in its chat, from a user explicitly asking “Please send this or these notes,” which is a valid use case that should not be prevented.

**How to apply:** evaluate supportive workflows as first-class editor interactions, not merely smaller autonomous runs. Preserve relevant request context without assuming every selection-based request belongs in the main conversation. Restrictions on autonomous initiative should not inadvertently prohibit explicitly user-directed note sending.

## Make interaction designs tangible in Emacs

User statements recorded 2026-11-10:

- Make design concepts tangible as an Emacs Lisp prototype—for example, a mockup buffer, non-functional commands, overlays, comments, and notes—so the user can assess how the interaction feels. A web/HTML sketch alone does not meet that evaluation goal.
- The user liked the child-frame chat window because it fits the “in-buffer workflow,” and liked note actions appearing on key press or when point moves onto them. Typing in the overlay must not accidentally trigger those actions. These are scoped presentation preferences, not a requirement to use child frames for every UI.

In the exchange captured by the November 11 journal (`segment-0001.chat.org`, turns 22–23), the user also said agents “can also add HTML artifacts as otuput.” HTML is allowed as additional output, not prohibited by the editor-native preference. Treating it as an optional supplement rather than a replacement for the requested Emacs Lisp prototype and DESIGN.md is the model's interpretation, not a separate quoted user requirement.

## Prefer direct requests over redundant conversation commands

User statements recorded 2026-11-13:

- A dedicated `/buddy` command to turn chat context into notes was redundant: with note tools available, the user can simply ask “please create notes from our chat.”
- “Re-ask” was redundant inside a note conversation because the user can type the question directly (`segment-0003.chat.org`, turns 257–258).

**Reusable preference:** when existing tools or ordinary conversation already express an action clearly, prefer that route over adding a single-purpose slash command or conversation action. This is not a prohibition on commands that provide a distinct interaction benefit.

The user also explained that each note is like a separate chat and may need a different preset or model depending on the workload (turns 256–257). Preserve workload-specific choice rather than assuming every note conversation must use one fixed model or preset. Current preset composition and session-identity mechanisms belong in maintained documentation and code.

## Note presentation preferences

User statements recorded 2026-11-13 (`segment-0003.chat.org`, turns 252 and 255):

- Derive note-face colors from the theme rather than hardcoded hex values.
- Hide note labels by default.
- Wrap long note text over multiple lines instead of letting it overflow on one line.
- Prefer the old directive-style inline action menu, opened by `M-m` with keys directly below the overlay, over a minibuffer `read-multiple-choice` menu; the user asked to bring most of that interaction back for notes.

Apply these preferences to note presentation, not as universal rules for every Emacs UI. No exact theme palette was selected; the offered swap of user/AI colors was not confirmed.

These statements establish user goals and preferences, not proof of current implementation or a settled storage, activation, or restart architecture. They do not establish the assistant's proposed “AI may speak unasked, acts only when you ask” rule as user-confirmed guidance.
