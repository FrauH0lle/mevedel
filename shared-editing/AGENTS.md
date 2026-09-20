# Shared editing

These instructions apply to `shared-editing/` and its descendants.

Read [the shared-editing contract](../docs/shared-editing.md) for runtime,
concurrency, editor isolation, host saves, and recovery behavior.

- The Node helper communicates privately with Emacs over stdin/stdout.
  Emacs owns durable reads and writes, including on remote execution targets.
- Edit source modules, then regenerate `host.bundle.mjs` and
  `relay/viewer/shared-editor.js` with the build script. Keep generated
  bundles and license notices with their source changes; include the
  lockfile when dependencies change.
- For changes to browser assets under `relay/`, also read
  [relay/AGENTS.md](../relay/AGENTS.md). Rebuild the relay to embed updated
  viewer assets.

## Build and checks

Use Node 22.4 or newer. From the repository root:

```sh
npm ci --prefix shared-editing
npm run build --prefix shared-editing
npm test --prefix shared-editing
npm run test:browser --prefix shared-editing
```

Install dependencies when setting up or after dependency changes. Rebuild
before browser checks, which exercise the generated viewer bundle.
See [Checks](../docs/shared-editing.md#checks) for browser prerequisites,
host-side ERT coverage, and remote acceptance checks. Elisp checks follow
[the development guide](../docs/development.md).
