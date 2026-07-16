# Bound automatic plan revision

Automatic Goals distinguish `revise` from `ask`: correctable plan defects
return to the planner even when several corrections or reasonable low-impact
choices are involved, while difficult decisions requiring user judgment
escalate immediately. Each plan-approval boundary permits at most two durable
revision rounds. The second replacement receives one final binary guardian
review: approval begins implementation, while any unresolved problem returns
control to the user. This absorbs ordinary review feedback without creating an
unbounded planner-reviewer negotiation.
