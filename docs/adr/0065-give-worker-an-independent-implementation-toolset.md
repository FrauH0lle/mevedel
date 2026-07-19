# Give worker an independent implementation toolset

Status: accepted

The built-in `worker` role defines a broad implementation toolset covering ordinary reading, editing, execution, code navigation, skills, tasks, and the full agent-control bundle, all bounded by root-session permissions. It does not intersect its tools with the delegator's direct tools. This lets a directly read-only `explorer` delegate implementation to a worker under the accepted transitive-authority model. An untyped `default` child continues to inherit its delegator's effective toolset exactly.
