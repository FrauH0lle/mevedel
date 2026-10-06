"""Forward MCP stdio bytes to a private socket in the owning Emacs process."""

import socket
import sys
import threading
import json


def hook(connection):
    """Return the owner's native hook decision before the agent continues."""
    connection.settimeout(30)
    event = json.load(sys.stdin)
    connection.sendall((json.dumps({"jsonrpc": "2.0", "id": 1,
                                   "method": "mevedel/control", "params": event})
                        + "\n").encode())
    with connection.makefile("r", encoding="utf-8") as stream:
        reply = json.loads(stream.readline(32 * 1024 * 1024 + 1))
    if reply.get("id") != 1 or "error" in reply or not isinstance(reply.get("result"), dict):
        raise ValueError("The owning turn could not answer the hook")
    print(json.dumps(reply["result"]))


def forward_input(connection):
    """Forward stdin independently so asynchronous server replies can flow."""
    try:
        while data := sys.stdin.buffer.read1(65536):
            connection.sendall(data)
        connection.shutdown(socket.SHUT_WR)
    except OSError:
        pass  # The main thread observes server closure and ends the bridge.


def main():
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as connection:
        connection.connect(sys.argv[1])
        if "--hook" in sys.argv[2:]:
            hook(connection)
            return
        threading.Thread(target=forward_input, args=(connection,), daemon=True).start()
        while data := connection.recv(65536):
            sys.stdout.buffer.write(data)
            sys.stdout.buffer.flush()


if __name__ == "__main__":
    try:
        main()
    except (OSError, IndexError, ValueError) as error:
        print(f"MCP bridge: {error}", file=sys.stderr)
        if "--hook" in sys.argv[2:]:
            # Native hook errors may otherwise be advisory.  Explicitly stop
            # when the owner cannot establish permission to continue.
            print(json.dumps({"continue": False,
                              "stopReason": "The owning mevedel turn is unavailable."}))
            sys.exit(0)
        sys.exit(1)
