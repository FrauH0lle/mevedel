# Fork effective post-compaction context

Status: accepted

`fork_turns` selects from the parent's current effective model-visible conversation rather than reconstructing raw history replaced by compaction. `all` includes current anchored summaries and all live turns; a positive count includes the summaries plus that many latest live turns; `none` includes neither. Mevedel assembles the child's instructions and role tools freshly, then appends the selected conversation snapshot and initial task.
