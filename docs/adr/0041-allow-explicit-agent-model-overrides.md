# Allow explicit agent model overrides

Status: accepted

`Agent` accepts optional `model` and `effort` fields. Resolution starts from the parent's model and effort, applies the selected role or workload policy, then applies explicit spawn overrides. Models should omit both fields unless the user, an invoked skill, or workspace instructions request an override. Mevedel reuses its existing model-policy resolver and does not add Codex's provider-specific `service_tier` field.
