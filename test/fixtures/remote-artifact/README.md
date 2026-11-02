# Remote-artifact frame fixtures

Recorded frame exchanges between a consumer of an artifact link and the
origin's artifact room. Both sides test against the same files: the origin's
ERT tests (`test/test-mevedel-collaboration-artifact-room.el`) check that the
origin produces them through the stub relay, and the remote-artifact
package's Node tests (`remote-artifact/test/`) replay them from a fake room.

Each file is one JSON object:

- `description`: what the exchange shows.
- `bindings`: example values for the `$NAME` placeholders. A string that is
  exactly a placeholder stands for that value; the origin tests bind the real
  ones (artifact id, title, lines), the fake room uses these examples. Where
  the origin's value cannot be known beforehand (changed lines, a picture,
  current hashes), the origin tests leave the placeholder unbound and it
  matches any string; they check those values' meaning separately.
- `tier` (optional): `"view"` or `"edit"`, the link tier the exchange needs.
  The fake room replays it only to a consumer joined at that tier.
- `frames`: the exchange in order, each `{"from": "consumer"|"origin",
  "frame": FRAME}`.

The exchanges are `hello`, `welcome`, `read`, `patch` (an edit link's
`SharedEdit` patch and its changed lines and picture), `stale-patch` (a stale
hash and the targets' current lines) and `view-only` (a view link's edit
refused).

A frame is the decoded JSON a sealed envelope carries, with one shorthand: an
editing request or reply, `{"t": "editing", "reqId": N, "body": VALUE}`,
stands for the chunked transfer `{"t": "editing", "reqId": N, "offset",
"total", "data"}` whose `data` slices join to base64 of VALUE's UTF-8 JSON.
Object key order is not significant.
