# Fetch Git over SSH through a host broker

Status: accepted

Related: [ADR 0017](0017-confine-protected-paths-by-default.md) (masked
credential paths such as `~/.ssh`) and
[ADR 0078](0078-default-to-best-effort-confinement.md) (best-effort
confinement). The [execution manual](../tools/execution.md) ("Git over SSH")
owns the model contract; [permissions](../permissions.md) describes the child
profile.

## Current decision

A confined Git command fetches over SSH through a **fetch-only broker** on the
host. Confinement itself never gains SSH keys, the ssh-agent, `known_hosts` or
SSH configuration.

- **When.** Local Bubblewrap confinement starts a per-command broker
  (`scripts/mevedel-git-ssh.py serve`) when the command text names `git` and
  the working directory's repository has fetch remotes that may use SSH. Every
  other command gets none.
- **How.** Inside confinement `GIT_SSH_COMMAND` runs the client half of the
  same script. It passes Git's argv and its own stdin, stdout and stderr
  descriptors over a Unix socket in a per-command owner-only directory. The
  socket root is masked; only this command's directory is bound back. The
  host broker runs the host's `ssh` on those descriptors with the user's agent
  and configuration, and exits with the confined command, stopping its ssh
  children.
- **What is allowed.** Only `git-upload-pack` and `git-upload-archive` in Git's
  exact quoting, with Git's own `-p`, `-4`/`-6` and `SendEnv` forms, for the
  SSH destinations (host and port) of the fetch remotes read on the host
  before the command starts. The host ssh argv is rebuilt from parsed values
  with `--` before the destination. Push (`git-receive-pack`), other remote
  commands, other ssh options and other hosts are refused before ssh runs.
- **Whose remotes count.** The allow-list comes only from Git metadata no
  confined command could have written first. A nested repository planted in a
  writable root gets no broker. *(Being finalized on the B5 branch after its
  security review; reconcile this bullet with the merged code.)*
- **The agent stays hidden.** Local confinement masks the socket named by
  `SSH_AUTH_SOCK` and unsets the variable.
- **Authority.** Fetch needs no network grant: the connection is the host's.
  It still needs the ordinary write grant for the repository's Git directory.
  HTTPS fetch is unchanged and still needs the network grant.

## Context

Before this decision `git fetch` over SSH could not run confined, so the only
path was `require_escalated`, which drops all confinement for a read-only
operation. Three independent blocks, observed on Arch with systemd's SSH proxy:

1. OpenSSH applies its owner check to `Include`d files. Under
   `--unshare-user`, `/etc/ssh/ssh_config.d/20-systemd-ssh-proxy.conf` appears
   owned by the overflow UID, so ssh aborts before connecting. An
   unprivileged namespace cannot map host root.
2. `~/.ssh` is masked by the default credential globs (ADR 0017), hiding
   `known_hosts` and `config` as well as keys.
3. `SSH_AUTH_SOCK` was not passed in.

While building the broker, a further fact surfaced: a Unix socket on a
read-only bind still accepts connections. An agent socket left visible would
let confined code sign with the user's keys, regardless of key masking.

## Options considered

- **Forward the agent socket and expose `known_hosts` and `config` read-only,
  keys still masked.** Leaves block 1 unsolved. Worse, the agent connection
  is signing authority for every host the keys open, for any purpose:
  confined code could push or log in elsewhere. Rejected.
- **A harness-owned view of `/etc/ssh` without the offending include.**
  Solves block 1 only. It changes the user's SSH behaviour inside confinement
  (the include may be needed for connectivity) and contradicts the execution
  manual's rule not to bypass configuration. Blocks 2 and 3 would still need
  the first option, with its agent risk. Rejected.
- **A fetch-only broker on the host.** Solves all three blocks at once,
  because ssh never runs confined. Keys and agent stay on the host, and push
  stays with a person. Chosen.

## Consequences

- Confined fetch, `pull --ff-only`, `ls-remote` and `archive --remote` work over
  SSH without escalation. The ssh_config ownership hint points Git fetches at
  the transport; other SSH operations keep the escalation advice.
- The ssh-agent is now hidden from **every** local confined command, not only
  Git. Anything that relied on an inherited agent inside confinement, such as
  a confined `ssh` or `scp`, now fails and needs the user's approval to
  escalate. This is intended: an agent connection is unbounded signing
  authority.
- Agent access is not forwarded in any form. The broker is the only path from
  confinement to the user's SSH identity, and it carries only fetch.
- A new host process runs per qualifying command. It is short-lived, local,
  and tied to the command's lifetime; its socket is reachable only through
  the command's own bind.
- **Known limits.**
  - Detection is textual: only a command whose text names `git` gets the
    broker. A wrapper or script that runs Git without `git` in the command
    text falls back to the old failure.
  - Local execution targets only. Remote targets get no broker; their SSH
    behaviour is unchanged.
  - Submodule remotes and repositories whose metadata a confined command could
    have planted get no broker.
  - A `.git` write grant lets a command change remotes, `insteadOf` rules and
    hooks that later brokers and host Git trust. The broker narrows *this*
    command's reach; it does not make granted `.git` writes harmless.
  - A hostile remote is out of scope: the broker limits which service runs
    where, not what the remote returns.
- Push stays a person's decision. A refusal is not a confinement failure to
  retry; escalating a push still needs the user's request.
