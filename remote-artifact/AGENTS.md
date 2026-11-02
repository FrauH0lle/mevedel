# Remote-artifact package

These instructions apply to `remote-artifact/` and its descendants.

The package is the consumer side of artifact links: an attach/detach CLI and
a stdio MCP server. The origin side is `mevedel-collaboration-artifact-room.el`;
see [artifact rooms](../docs/collaboration.md#artifact-rooms) for the room's
contract.

- Keep it dependency-free: Node 22.4+ built-ins only (global `WebSocket`,
  WebCrypto, `node:` modules). No Elisp dependency, no CRDT code; the origin
  executes every action.
- Links are secrets. Only `attach` takes one, and nothing prints it.
- Tools are entries in `src/tools.mjs`; room connections come from
  `ctx.connect`, which `serve` backs with the idle-closing pool in
  `src/connections.mjs`; a tool calls `close()` on the room to release it.
- Protocol changes go with the origin and the shared frame fixtures in
  `test/fixtures/remote-artifact/`: the origin's ERT tests check it produces
  them, the fake room in `test/fake-room.mjs` replays them.

## Checks

From the repository root:

```sh
npm test --prefix remote-artifact
```

Origin-side changes also run
`python3 test/run_tests.py test/test-mevedel-collaboration-artifact-room.el`.
Before merging, run the end-to-end check with a real relay, origin and browser:
`node --test test/remote-artifact.browser.mjs` (see
[artifact-link acceptance](../docs/development.md#artifact-link-acceptance)).
