# Session cockpit owns view status

The mevedel view buffer presents mevedel-owned status and menu entry
points instead of proxying gptel's clickable header line. The raw data buffer
may keep gptel's normal header, while the view routes mode, model, tools,
skills, plugins, worktree, and help through the session cockpit. This lets
mevedel replace broad gptel transient retargeting advice with a narrow bridge
for explicitly opening `gptel-menu` from the paired data buffer.

Inspection rows above the composer expand locally rather than opening a cockpit
as a side effect. Named management actions still open the owning cockpit. In the
status strip, Goal status/turns and model name have separate links: the model
label always opens Model, even when a Goal exists. Previously the combined
Goal/model label changed the model's destination to Goal; separating them keeps
each label's action predictable.
