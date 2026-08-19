# mevedel documentation

mevedel is an Emacs Lisp package that provides a visual, overlay-based
workflow for interacting with LLMs while programming, with direct gptel
integration.

This site publishes the `docs/` tree of the repository. It is the maintained
working documentation, written for humans and for agents working in the
codebase; the sidebar lists every page. The repository root
[`CLAUDE.md`](https://github.com/FrauH0lle/mevedel/blob/master/CLAUDE.md) is
the agent entry point and holds the annotated documentation map and the module
layer map.

Good starting points:

- [Architecture](architecture.md) — data structures, workspace context chain,
  gptel integration, persistence layout
- [Tools](tools.md) and [Permissions](permissions.md) — the tool pipeline and
  the permission decision chain
- [Agents](agents.md) — worker, explorer, verifier, reviewer
- [Architecture Decision Records](adr/README.md) — why the design is what it is
