---
name: "Completion-time hook reminders cannot change the finished turn"
description: "Why the project Stop-hook validation reminder was deleted rather than relocated"
type: project
---

# Completion-time hook reminders cannot change the finished turn

Observed 2026-09-18 (session `segment-0001.chat.org`, turn 4): the project `Stop` entry was removed from `.mevedel/hooks.json` along with `stop_validation_context()` and its `stop-validation-context` dispatch entry in `.mevedel/hooks/policy.py`, and `test_configured_handlers_accept_event_payload` was added to `test/test_project_hook_policy.py` to exercise every remaining configured helper. `npx @emacs-eask/cli clean elc` ran and `python3 -B test/test_project_hook_policy.py` passed all 5 tests; the hook engine and standing validation policy were unchanged.

Model inference supported by inspected source (turn 1/3; `mevedel-turn.el:487–506`, `mevedel-hooks.el:1268–1278,1563–1581`): root `Stop` runs after the answer is complete and uses `#'ignore` for its decision callback, so the removed helper's context-only result could neither change the completed answer nor queue guidance for the next prompt.

- **User-approved decision** (turn 3, approved turn 4): delete the reminder rather than relocate it. Moving it to `UserPromptSubmit` was rejected as duplication of `prompts/system/task-policy.md:43–47` and `prompts/tones/main.md:15–17`; converting it to a notification was rejected because the helper unconditionally emitted an "If…" instruction and never detected changed files, executed checks, or unsupported completion claims.
- The former description, "Surface missing validation before completion", contradicted the actual timing and behavior. Deleting it removed ineffective automation, not enforcement.
- **How to apply:** if the goal is to influence the current turn's output or the next prompt, put the logic somewhere it runs then. Do not re-add a completion-time reminder hook and expect it to enforce validation.
- **Activation caveat:** editing hook configuration does not reload a running Emacs. Live config and trust state were not reloaded or modified; content-hash trust and cache invalidation remained an activation concern, and the handoff naming `M-x mevedel-hooks-trust-project` recorded no activation outcome.

## Related engine note

When touching hook lifetimes, verify in source rather than from memory: request-owned command hooks cancel without resuming an abandoned continuation, while `Stop`/`StopFailure` retain request rules but outlive request teardown under their own timeout (`mevedel-hooks.el:1880–1897,2292–2297`), and hooks matching `TaskUpdate` miss automatic task completion through `mevedel-tool-task-finalize-owner` (`mevedel-tool-task.el:349–386`). The backlog records domain-level task transitions as justified only when an integration needs every completion.
