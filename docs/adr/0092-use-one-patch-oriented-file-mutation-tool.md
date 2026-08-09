# Use one patch-oriented file mutation tool

Replace separate Edit, Write, and MkDir model tools with one `ApplyPatch` tool using Codex's patch grammar. A single multi-file patch proposal preserves the model's intended change set and enables one hierarchical, atomic patch review with per-change decisions; the trade-off is that permissions, snapshots, and preview state must understand every affected path rather than one file at a time.
