---
name: "Subscription quota integration sources"
description: "Authoritative Codex, Claude SDK, and ACP pointers for subscription usage research"
type: reference
---

# Subscription quota integration sources

Source investigation recorded **2026-10-07** in a temporary research note. The investigation was read-only: it reported no authenticated calls or live quota requests. Recheck current contracts and installed versions before using these references.

## Codex

- [Official app-server account rate-limit documentation](https://learn.chatgpt.com/docs/app-server#6-rate-limits-chatgpt): starting point for subscription quota integration and the documented account rate-limit interface.
- [Official app-server token-usage documentation](https://learn.chatgpt.com/docs/app-server#7-token-usage-chatgpt): consult separately for account token activity; do not assume token activity is remaining subscription allowance.

## Claude SDK and ACP

- [Published Claude Agent SDK 0.3.287 declarations](https://unpkg.com/@anthropic-ai/claude-agent-sdk@0.3.287/sdk.d.ts): dated first-party source for the experimental structured usage interface. The note reports that the interface explicitly warns it may change or disappear; this version is a research anchor, not a stable integration guarantee.
- [Claude ACP adapter v0.86.0 command implementation](https://github.com/agentclientprotocol/claude-agent-acp/blob/v0.86.0/src/acp-agent.ts) and [usage renderer](https://github.com/agentclientprotocol/claude-agent-acp/blob/v0.86.0/src/usage-markdown.ts): dated implementation references for `/usage` handling and structured usage presentation. Check the installed agent's advertised commands rather than assuming this release's behavior.
- [Standard ACP UsageUpdate](https://agentclientprotocol.github.io/typescript-sdk/types/UsageUpdate.html): use to distinguish the standard protocol contract from adapter-specific extensions.
- [Claude Code status-line documentation](https://code.claude.com/docs/en/statusline#available-data): reference for passive status-line usage data and its availability conditions.
- [Claude platform Usage and Cost API](https://platform.claude.com/docs/en/manage-claude/usage-cost-api): reference for organization API consumption and costs, which the research distinguishes from personal subscription quotas.

The note also examined `~/ccs`, whose README described a mirror of a source-map snapshot exposed on 2026-03-31, not an official current Anthropic repository. Do not treat that older snapshot as authoritative evidence of current SDK or ACP capabilities.
