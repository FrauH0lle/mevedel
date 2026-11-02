# mevedel-remote-artifact

Work on whiteboards and documents that live in a
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

## Use it from an MCP client

```sh
claude mcp add remote-artifacts -- npx mevedel-remote-artifact mcp
```

Tools:

- `list_artifacts`: each attached artifact's name, title, kind, link tier
  (view or edit) and whether its origin is reachable.
- `read_artifact(name)`: the whiteboard or document as hash-prefixed lines,
  exactly as `shared://ID` shows it at the origin.

When the origin is offline, or the link was revoked or the artifact deleted,
tools fail with "Origin unavailable (revoked, deleted, or offline)".

## Licence

GPL-3.0-or-later, like mevedel.
