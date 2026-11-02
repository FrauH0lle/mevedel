"""Landlock launcher that limits which host Unix sockets a confined command reaches.

`abi` prints the running kernel's Landlock ABI version, or 0 when Landlock is
unavailable.  Pathname socket restriction needs version 9
(LANDLOCK_ACCESS_FS_RESOLVE_UNIX).

`exec [ALLOWED...] -- COMMAND...` runs inside Bubblewrap.  It restricts this
process and every descendant so that connecting to, or sending a datagram to,
a pathname Unix socket created outside the new domain succeeds only beneath
an ALLOWED path, and connecting to an abstract socket created outside the
domain fails, then execs COMMAND.  Sockets the command or its descendants
create stay reachable.  No other filesystem or network access is handled, so
Bubblewrap's mounts remain the filesystem boundary.  An ALLOWED path that
cannot be opened gets no rule, which leaves its sockets unreachable.  Any
other failure exits with status 125 before COMMAND starts.

Out of scope, as for the Git SSH broker: a writable development checkout of
this script.  Emacs runs `abi` on the host, so a confined edit to it in a
workspace that is the mevedel checkout runs as host code.
"""

import ctypes
import os
import struct
import sys

# The Landlock system calls share one number on every architecture but Alpha.
SYS_CREATE_RULESET = 444
SYS_ADD_RULE = 445
SYS_RESTRICT_SELF = 446
CREATE_RULESET_VERSION = 1 << 0
RULE_PATH_BENEATH = 1
ACCESS_FS_RESOLVE_UNIX = 1 << 16
SCOPE_ABSTRACT_UNIX_SOCKET = 1 << 0
PR_SET_NO_NEW_PRIVS = 38
RESOLVE_UNIX_ABI = 9

LIBC = ctypes.CDLL(None, use_errno=True)
LIBC.syscall.restype = ctypes.c_long


def call(number, *arguments):
    """Return system call NUMBER's result, raising OSError on failure."""
    result = LIBC.syscall(ctypes.c_long(number), *arguments)
    if result < 0:
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error))
    return result


def abi():
    """Return the running kernel's Landlock ABI version, 0 when unavailable."""
    try:
        return call(SYS_CREATE_RULESET, None, ctypes.c_size_t(0),
                    ctypes.c_uint32(CREATE_RULESET_VERSION))
    except OSError:
        return 0


def restrict(allowed):
    """Restrict this process to host Unix sockets beneath ALLOWED paths."""
    # struct landlock_ruleset_attr: handled_access_fs, handled_access_net, scoped.
    attributes = ctypes.create_string_buffer(
        struct.pack("=QQQ", ACCESS_FS_RESOLVE_UNIX, 0,
                    SCOPE_ABSTRACT_UNIX_SOCKET))
    ruleset = call(SYS_CREATE_RULESET, attributes,
                   ctypes.c_size_t(len(attributes.raw)), ctypes.c_uint32(0))
    try:
        for path in allowed:
            try:
                descriptor = os.open(path, os.O_PATH | os.O_CLOEXEC)
            except OSError:
                continue
            try:
                # struct landlock_path_beneath_attr is packed.
                rule = ctypes.create_string_buffer(
                    struct.pack("=Qi", ACCESS_FS_RESOLVE_UNIX, descriptor))
                call(SYS_ADD_RULE, ctypes.c_int(ruleset),
                     ctypes.c_int(RULE_PATH_BENEATH), rule, ctypes.c_uint32(0))
            finally:
                os.close(descriptor)
        if LIBC.prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0:
            error = ctypes.get_errno()
            raise OSError(error, os.strerror(error))
        call(SYS_RESTRICT_SELF, ctypes.c_int(ruleset), ctypes.c_uint32(0))
    finally:
        os.close(ruleset)


def main(argv):
    """Run the launcher subcommand in ARGV; return an exit status on failure."""
    if argv[1:] == ["abi"]:
        print(abi())
        return 0
    if len(argv) < 4 or argv[1] != "exec" or "--" not in argv[2:-1]:
        sys.stderr.write("usage: mevedel-landlock.py abi | exec [ALLOWED...] -- COMMAND...\n")
        return 2
    separator = argv.index("--", 2)
    try:
        restrict(argv[2:separator])
    except OSError as error:
        sys.stderr.write(f"mevedel-landlock: cannot restrict Unix sockets: {error}\n")
        return 125
    command = argv[separator + 1:]
    try:
        os.execvp(command[0], command)
    except OSError as error:
        sys.stderr.write(f"mevedel-landlock: {command[0]}: {error}\n")
        return 127


if __name__ == "__main__":
    sys.exit(main(sys.argv))
