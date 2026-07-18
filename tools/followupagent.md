Continue an existing retained agent conversation.

`target` accepts a canonical path such as `/root/spec_review` or a relative
descendant path beneath the caller. `message` is the complete follow-up task.
An idle target starts another turn when capacity is available. A running target
receives the task at the next safe boundary without consuming another slot.

Success returns an empty result. Every eventual terminal RESULT still goes to
the target's original spawn parent.
