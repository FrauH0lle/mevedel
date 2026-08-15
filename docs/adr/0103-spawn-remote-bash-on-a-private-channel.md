# Spawn remote Bash on a private channel

Status: accepted

## Decision

An eligible remote Bash execution spawns through TRAMP's direct
asynchronous process path: a local `ssh` (or container client) invocation
of its own, carrying `cd` into the working directory and the execution
environment, instead of a handler spawn multiplexed over the shared
control connection.  Eligible means: the option is on (its default), the
execution is not a TTY, the target is a single-hop ssh or scp
connection, and the wrapped command fits the remote pipe buffer with
margin.  Everything else -- TTY executions, oversized commands, hops,
container and other methods -- keeps the classic shared-channel spawn,
decided per record at launch.  The container methods carry the
direct-async parameter too, but their per-spawn client exec allocates
a tty and prints its own notices ahead of the command; carriage
returns and interleaved client output corrupt the group marker
protocol, so they stay classic until that protocol tolerates them.

The channel is forced per spawn rather than through a connection-local
profile.  A profile flips every `make-process` on that host for the whole
Emacs session, including other packages' processes; forcing the TRAMP
predicate around mevedel's own spawn changes exactly one process, in both
directions -- a TTY spawn stays classic even on a connection where
something else enabled direct-async.

For ssh and scp the direct-async connection property is set to t, which
drops the method's default remote pty allocation.  The pty would mangle
binary output; nothing in the stop path depends on it, because signals
reach the process group by number over the control connection, through
the group marker captured at launch -- direct-async never sets a
`remote-pid`, and `interrupt-process` never reaches the remote job, but
mevedel never used either.

The execution environment travels as an explicit `env` prefix inside the
group wrapper.  TRAMP transfers only the difference between the current
and the top-level `process-environment`, which silently drops any default
the user's own environment happens to share; the prefix is deterministic.

## Consequences

A live remote Bash no longer occupies the shared control connection for
its duration.  Saves, leases, publication, and stop probes stop
serializing behind long-running executions, and the execution stops being
the reentrancy window the transport layer's busy predicate documents as
its blind spot -- that blind spot now covers only classic spawns.

Each direct-async spawn is its own ssh process: authentication must be
non-interactive (an agent, a key, or connection sharing), and a spawn
without socket sharing pays a handshake.  Emacs 30.2 ships a defect
here: the spawn asks for its ssh options through a function TRAMP
renamed, silently receiving none -- which loses not only ControlMaster
sharing but any option routed through the same-named variable, such as
the -F config a host alias needs to resolve at all.  mevedel restores
the intended call with an alias before the first ssh direct-async
spawn, so the spawn carries the user's options and reuses the master
socket while still running outside the master shell -- the shell
channel is the contention this decision removes.

The local process's exit status is the ssh client's: a remote exit 255 is
indistinguishable from a transport failure.  The zombie-aware group probe
arbitrates the cases that matter -- group settled versus group alive --
so the ambiguity costs a probe, not a wrong outcome.

The public execution facts carry `:direct-async`, so a transcript records
which channel a command ran on, and the acceptance suite asserts the
private channel is actually taken for eligible spawns rather than
silently falling back.
