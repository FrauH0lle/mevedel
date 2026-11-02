# mevedel-remote-artifact

Work on whiteboards, documents and HTML artifacts that live in a
[mevedel](https://github.com/FrauH0lle/mevedel) workspace on another machine
(their *origin*) from Claude Code, Codex or any MCP client. The origin shares
an artifact and gives you an *artifact link*; you attach the link here, and
your agent reads the artifact through MCP tools. Every read goes live to the
origin; nothing is copied. Needs Node 22.4 or newer and nothing else.

## Attach a link

Attach links in a terminal, never in a chat: a link is a secret.

```sh
npx mevedel-remote-artifact attach 'https://relay.example/#ROOM.SECRET' [--name NAME] [--as DISPLAY-NAME]
npx mevedel-remote-artifact detach NAME
```

An attached artifact is named by its id at the origin unless `--name`
chooses another; attaching refuses a name that is already taken. `--as` sets
the name the origin shows for you (default `user@machine (client)`). Session
and lobby links are refused. Links are stored in one file per user,
`$XDG_CONFIG_HOME/mevedel-remote-artifact/links.json` (default
`~/.config/...`), readable only by you.

## Install and set up a client

Nothing to install beyond Node 22.4 or newer: `npx` fetches the package on
first use. Attach a link first (above), then register the MCP server once.

Claude Code:

```sh
claude mcp add remote-artifacts -- npx mevedel-remote-artifact mcp
```

Codex (`codex mcp add remote-artifacts -- npx mevedel-remote-artifact mcp`, or
in `~/.codex/config.toml`):

```toml
[mcp_servers.remote-artifacts]
command = "npx"
args = ["mevedel-remote-artifact", "mcp"]
```

Any other MCP client: run `npx mevedel-remote-artifact mcp` as a stdio server.
To install globally instead, `npm install -g mevedel-remote-artifact` and use
`mevedel-remote-artifact mcp` as the command.

## Connection lifecycle

The server connects to an artifact's origin the first time a tool call needs
it, reuses the connection for later calls, and closes it after 5 idle minutes
so an open client does not keep the origin awake. The next call reconnects
transparently. If the origin speaks another protocol version, tools fail with
"origin speaks protocol N, this package speaks M; update one": update the
package (or the origin's mevedel) so both match.

Tools:

- `list_artifacts`: each attached artifact's name, title, kind, origin (the
  workspace and host the origin names in its welcome, as attach stored it or
  as a reachable origin names it now), link tier (view or edit) and whether
  its origin is reachable.
- `read_artifact(name, file?, history?)`: the whiteboard or document as
  hash-prefixed lines, exactly as `shared://ID` shows it at the origin. With
  `history: true`, its recent contributions instead, as `shared://ID/history`
  shows them, each with the id `revert` names. For an HTML artifact, one file:
  the primary page unless `file` names another, text as is, PNG, JPEG, GIF
  and WebP files as images, other bytes as a note of their size. Any link may
  read.
- `view_artifact(name, image?, item?)`: the whiteboard as drawn, or with
  `image` one embedded image (a board image by its `fileId`, a document image
  by the key its `src` shows), or with `item` the preview of one library item
  of a whiteboard (`LIBRARY/ITEM-ID` from `whiteboard_libraries`; not with
  `image`).
- `edit_artifact(name, action, ...)`: `patch`, `image`, `rename`,
  `background`, `insert` and `revert` with mevedel `SharedEdit`'s argument
  names and results: the stored lines of what changed and, for a whiteboard,
  the picture. A stale
  hash rejects the patch and returns its targets' current lines. `image`
  reads `source`, a local PNG, JPEG, WebP or SVG file of at most 4.7 MB
  (relative paths resolve against the MCP server's working directory), and
  refuses anything else before contacting the origin. `insert` places a
  library item (`item` as `LIBRARY/ITEM-ID`, at `x`,`y`) on a whiteboard.
  `revert` undoes the contribution `transaction` names and is refused when
  its targets changed since. A view link's edit fails with "This artifact
  link is view-only".
- `whiteboard_libraries(name, library?, preview?)`: the element libraries of
  the user who shared a whiteboard (their own, installed and built-in) as a
  listing of `LIBRARY/ITEM-ID` refs, or with `preview` the picture of one
  library. Any link may browse; libraries cannot be changed through a link,
  and document links get no libraries. Sharing a whiteboard exposes the
  origin user's libraries.
- `list_artifact_files(name)`: an HTML artifact's files with their sizes,
  the primary one marked. Any link may list.
- `edit_artifact_file(name, file, old_string, new_string)`: replace one
  exact, unique occurrence in a text file of an HTML artifact.
- `write_artifact_file(name, file, content)`: replace a whole text file, or
  create it with any missing directories. Files are limited to 16 MiB.
- `delete_artifact_file(name, file)`: delete a file, never the primary one.

The file tools need an edit link, and every change except creating a file
needs a `read_artifact` of that file first: the server remembers the SHA-256
of what was read, and the origin writes only if the file still has it. A file
changed at the origin since is not written; the tool fails with the file's
current content (under 256 KiB) to redo the change on, which also counts as
the new read. Creating a file that exists fails the same way. Paths are
relative to the artifact's directory; `..`, absolute paths and symbolic links
are refused with "Refused path ...".

Edits are attributed at the origin as `Agent: NAME (remote)`, NAME being the
display name given at attach (`--as`) or the default user, machine and
client. The origin can revert them, lists a connected consumer by that name
on the artifact, and records one version when a stretch of edits ends (5
minutes without edits, or the connection closing); for an HTML artifact, when
the stretch wrote its primary file. Versions themselves stay with the origin:
the package cannot list, save or restore them.

When the origin is offline, or the link was revoked or the artifact deleted,
tools fail with "Origin unavailable (revoked, deleted, or offline)". An origin
that ends the connection says why, and tools report it: the host revoked the
link (ask for a new one), the artifact was deleted, or the origin Emacs
stopped (try again once it is back).

## The bridge for mevedel

mevedel uses this package as its remote-artifact backend: `M-x
mevedel-remote-artifact-attach` keeps the link in mevedel's own private
record and runs `mevedel-remote-artifact bridge` as a subprocess. The bridge
is a machine interface, not a command for people. It reads one JSON request
per line on standard input and answers one JSON line each on standard output:

```text
in   {"id", "link", "guestId", "name", "ref"?, "body"?}
out  {"id", "welcome": {"id", "kind", "title", "readOnly"}, "reply"?}
out  {"id", "error"}
out  {"ref", "event"}
```

A request without `body` only connects and returns the artifact's welcome;
with one, `body` is an artifact-room editing request (`read`, `view`, `patch`
and the other actions above, with the origin's argument names), pinned to the
room's own artifact id, and `reply` is the origin's answer. `name` is the
display name the origin attributes edits to; each distinct link and name has
its own pooled connection with the lifecycle above. Origin change events on
a connection are written with the `ref` of the request that opened it. The
link travels only over the pipe and is never written out.

## Licence

GPL-3.0-or-later, like mevedel (`LICENSE`). The package has no dependencies
and bundles no third-party code (`NOTICE.md`).
