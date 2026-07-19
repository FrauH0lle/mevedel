# Fork an explicit parent context snapshot

Status: accepted

`Agent` accepts one optional `fork_turns` string: `all` copies the parent's complete effective post-compaction conversation and is the default, `none` starts from instructions, role configuration, and the initial task only, and a positive integer copies anchored summaries plus that many recent live parent turns. The fork is an immutable snapshot rather than a synchronized conversation and never reconstructs raw compacted turns. The initial task is always added after the copied context. V2 removes `run_in_background` because every agent turn is asynchronous.
