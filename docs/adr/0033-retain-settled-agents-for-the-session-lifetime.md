# Retain settled agents for the session lifetime

Status: accepted

A settled agent remains addressable, with its conversation intact, until its root session is deleted. It consumes no agent-turn capacity while idle. Persisting and resuming the root session therefore preserves its agent tree. Mevedel will not add an explicit close operation or independent agent garbage collection until retained agents create a demonstrated resource problem.
