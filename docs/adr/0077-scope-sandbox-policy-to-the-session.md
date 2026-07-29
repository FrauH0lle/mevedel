# Scope sandbox policy to the session

Status: accepted

Sandbox mode is a persisted session setting, initially copied from a global
default for new sessions. Confinement policy expresses risk tolerance for one
workflow rather than a host-wide fact, so concurrent sessions may independently
choose `best-effort`, `required`, or `off`.
