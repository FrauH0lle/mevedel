# Agent roles are optional overlays

Status: accepted

The `Agent` tool takes an optional `role`. Omitting the role creates a default child that inherits the delegating agent's effective instructions, tools, model policy, and delegation capability while sharing the root session's dynamic permission policy; a named role applies its own specialization as a configuration overlay. Roles therefore guide or specialize agents without creating distinct runtime classes, allowing homogeneous recursive swarms without a duplicated built-in worker runtime.

## Decision history

A required `subagent_type` selector was replaced with optional role selection.
Default inheritance supports homogeneous recursive delegation without a separate
worker runtime; named roles supply their own specialization.
