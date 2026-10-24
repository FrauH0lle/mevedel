# Local reference sources

Use this map to locate reference implementations before attributing behavior to
Codex or Claude Code. It complements the [development guide](development.md),
not its gptel source rule. These are machine-local references, not mevedel
dependencies or portable checkout requirements. `~` below means
`/home/roland` on the checked machine.

**Last checked: 2026-10-24.** Every listed source/artifact path existed at that
check. Recheck identity before relying on behavior: a source snapshot, an
installed CLI, an adapter package, and a running session are different evidence.
Do not assume the newest installed runtime is the one a session uses.

## Codex checkout

Root: `~/codex`. Clean Git checkout at
`8c076942cb70fba8e5c4732c23f1faabc4aed484` (commit date 2026-10-18).
The workspace version in `codex-rs/Cargo.toml` is `0.0.0`; use the Git revision
as the source identity, not that placeholder. No `codex` executable was on the
checked shell's `PATH`.

Paths below are relative to `~/codex/codex-rs/core/src/tools/`:

| Question | Source entry point |
| --- | --- |
| Tool inventory, schemas, exposure and registration | `spec_plan.rs`: `build_tool_router`, `add_core_tool_sources`; assembles core, MCP, extension and dynamic tools. |
| Registration and execution lookup | `registry.rs`: `ToolRegistry`, `register_trusted_with_exposure`, `register_external_with_exposure`, `dispatch_any_with_state`. |
| Model-call parsing and routing | `router.rs`: `ToolRouter::build_tool_call`, `dispatch_tool_call_with_state`. |
| Concrete handlers | `handlers/apply_patch.rs`, `handlers/unified_exec/exec_command.rs`, `handlers/unified_exec/write_stdin.rs`; retained-agent messaging example: `handlers/multi_agents_v2/send_message.rs`. |

Read-only identity checks (no fetch or build required):

```sh
git -C ~/codex rev-parse HEAD
git -C ~/codex status --short
git -C ~/codex describe --tags --always --dirty
```

## Claude Code source snapshot

Root: `~/ccs`. This contains readable TypeScript implementation source, not
merely CLI documentation. Its `README.md` describes a research mirror of a
source-map exposure, not an official Anthropic repository; that provenance
claim was not independently authenticated. `package.json` reports
`@anthropic-ai/claude-code` version `1.0.0-research`, not the installed product
version. Git is initialized but has **no HEAD commit**; the files are untracked.
Do not cite an invented revision or treat it as source matching today's CLI.

Paths below are relative to `~/ccs/`:

| Question | Source entry point |
| --- | --- |
| Tool registration/availability | `src/tools.ts`: `getAllBaseTools`, `getToolsForDefaultPreset`; includes feature/environment gates. |
| Tool contract and context | `src/Tool.ts`: tool types, `ToolUseContext`, permission context. |
| Execution dispatch | `src/services/tools/toolExecution.ts`: invokes `tool.call` after input/permission processing. |
| Concrete handlers | `src/tools/BashTool/BashTool.tsx`: `BashTool.call`; `src/tools/AgentTool/AgentTool.tsx`: `AgentTool.call`. |

Without a commit, record content hashes for the files actually used. Checked
SHA-256 fingerprints:

| File | SHA-256 |
| --- | --- |
| `package.json` | `0dbda1263b7b049eefa0691d05c326e41a6620bc6958bcae68d06b372d174cc2` |
| `src/tools.ts` | `4307f3f7dec2916299cc2fa5c45df8b95fd9a1945be7ac918ccd3d47a42f18cb` |
| `src/Tool.ts` | `29d555ab8e50de9fb005264b5334aba870f1da472198e14d0b3ab0bf86d7c49c` |

```sh
git -C ~/ccs rev-parse --verify HEAD # Currently fails: no commit.
git -C ~/ccs status --short
node -p 'JSON.parse(require("fs").readFileSync(process.env.HOME + "/ccs/package.json", "utf8")).version'
sha256sum ~/ccs/package.json ~/ccs/src/tools.ts ~/ccs/src/Tool.ts
```

These hashes identify selected files only, not the entire snapshot. Reading
source establishes implementation evidence; this check did not build or run it.

## Installed Claude Code CLI

Shell executable: `~/.local/bin/claude`, resolving to
`~/.local/share/claude/versions/2.1.310`. `claude --version` returned
`2.1.310 (Claude Code)`. `file` identified an x86-64 Linux ELF executable, not
a readable TypeScript checkout. SHA-256 of the resolved executable:
`b73676430af12f97316a798b8371f81d640f90a58bdcb8f3ce56a835bb47df56`.

```sh
command -v claude
readlink -f "$(command -v claude)"
claude --version
file "$(readlink -f "$(command -v claude)")"
sha256sum "$(readlink -f "$(command -v claude)")"
```

Tool registration/handler source for this exact native build was not established.
Use the snapshot above for qualified implementation comparisons, and observed
CLI behavior for claims about this installation; do not claim they match.

## Installed ACP adapter runtimes

Runtime root: `~/.emacs.d/.local/etc/mevedel/claude-code/runtimes`.
Both `0.94.0` and `0.95.0` exist, each with `package.json`, `package-lock.json`
and `node_modules/@agentclientprotocol/claude-agent-acp/package.json`.
Installed versions were read from package manifests and adapter `--version`;
bundled CLI versions were also executed with the executable override unset:

| Runtime / adapter | Claude Agent SDK | SDK-declared / executed bundled Claude CLI | ACP SDK |
| --- | --- | --- | --- |
| `0.94.0` | `0.3.303` | `2.1.303` | `2.0.0` |
| `0.95.0` | `0.3.309` | `2.1.309` | `2.0.1` |

For each runtime, the SDK manifest is
`node_modules/@anthropic-ai/claude-agent-sdk/package.json`; the ACP SDK manifest
is `node_modules/@agentclientprotocol/sdk/package.json`. The native SDK CLI is
`node_modules/@anthropic-ai/claude-agent-sdk-linux-x64/claude`.

The following implementation map was inspected in **0.95.0**, relative to
`node_modules/@agentclientprotocol/claude-agent-acp/`. Readable compiled
JavaScript and declarations are installed; raw TypeScript sources are not
included in these two adapter package directories.

| Question | Installed implementation entry point |
| --- | --- |
| ACP entry / version handling | `dist/index.js`: adapter `--version` versus native delegation via `--cli`. |
| Execution backend and permission handling | `dist/acp-agent.js`: `claudeCliPath`, SDK `query` options (`tools`, `disallowedTools`, `mcpServers`, `canUseTool`), `canUseTool(sessionId)`. |
| Tool-report conversion | `dist/tools.js`: `toolInfoFromToolUse`, `toolUpdateFromToolResult`. |
| Tool-name reporter registration / rendering | `dist/tool-calls/reporters/index.js`: `reporters`, `reporterFor`; `dist/tool-calls/renderer.js`: `AcpToolCallRenderer`. These render reports, not native execution handlers. |

The adapter normally resolves the SDK's platform-specific native CLI, **not**
the shell's `claude`. `CLAUDE_CODE_EXECUTABLE` overrides that resolution. The
version matrix does not establish a running session's selected executable or
environment. The adapter is not a source checkout of native Claude tool handlers.

Read-only checks, executed with Node `v26.12.0` at the last check:

```sh
R="$HOME/.emacs.d/.local/etc/mevedel/claude-code/runtimes"
for V in 0.94.0 0.95.0; do
  A="$R/$V/node_modules/@agentclientprotocol/claude-agent-acp"
  node "$A/dist/index.js" --version
  env -u CLAUDE_CODE_EXECUTABLE node "$A/dist/index.js" --cli --version
  sha256sum "$A/dist/acp-agent.js"
done
```

Checked SHA-256 of `dist/acp-agent.js`: `0.94.0` =
`4fc4f4902ad4859666620635963abec6eb87263cf78b9fd4adb11708620745a4`;
`0.95.0` =
`be0ebf68bb422a7c0867db8092e99e601eca0390d22f33f385f76b123e296243`.
The lockfiles also record package resolution and integrity. These checks made
no model requests, installed nothing, and did not authenticate upstream
provenance or validate tool behavior end to end. Refresh this map's date,
identities and locators together when the local references change.
