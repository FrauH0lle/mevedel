---
name: "Completion-time reminders cannot enforce a finished turn"
description: "Place guidance where it can affect the intended turn; do not mistake a context-only completion hook for enforcement"
type: feedback
---

# Completion-time reminders cannot enforce a finished turn

On 2026-09-18, the user approved deleting the project Stop-hook validation reminder rather than relocating it.

The recorded source investigation concluded that this reminder ran after the answer was complete and its context-only result could neither change that answer nor queue guidance for the next prompt. Its description, “Surface missing validation before completion,” therefore overstated what it did. Removing it removed ineffective automation, not validation enforcement.

The alternatives were rejected for distinct reasons:

- Moving it to prompt submission would duplicate existing task-policy guidance.
- Turning it into a notification would still not detect changed files, executed checks, or unsupported completion claims: the helper emitted an unconditional conditional instruction rather than checking those facts.

## Apply when designing reminders or enforcement

Check the lifecycle timing and how the result is consumed before claiming that a hook influences an answer. Guidance must run where it can affect the intended turn; enforcement requires an actual check and a consequential result path.

Do not re-add a context-only completion-time reminder expecting it to enforce validation. This lesson is about the removed reminder's behavior, not a claim that every completion hook is ineffective. Consult current source and maintained hook documentation for engine semantics.
