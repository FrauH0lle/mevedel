# Full-auto authorizes additive network access

Status: superseded by ADR 0086

`Full-auto` authorizes a model-requested additive network capability without a
prompt, while `ask` and `edits` require direct user authority. The model must
still request network access explicitly with a justification; mevedel does not
infer, replay, or widen a default network-isolated invocation. This keeps
ordinary execution confined while allowing unattended workflows to opt into
network access without interaction.
