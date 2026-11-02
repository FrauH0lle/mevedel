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

The exchanges are `hello`, `welcome`, `read`, `history` (a read of the recent
contributions), `patch` (an edit link's `SharedEdit` patch and its changed
lines and picture), `stale-patch` (a stale hash and the targets' current
lines), `view-only` (a view link's edit refused), `revert` (an edit link
reverting one listed contribution), and for HTML artifacts `read-file` (one
file read), `write-file` (a hash-checked file write) and `write-conflict` (a
write refused because the file changed).

## HTML file writes

An HTML artifact has one write, which every consumer uses, the package's
file tools and a mevedel consumer's `ApplyPatch` alike: replace or create a
whole file if its SHA-256 still matches. The request body is

```json
{"action": "write-file", "id": "ID", "file": "assets/site.css",
 "hash": "SHA-256 HEX OR null", "text": "CONTENT"}
```

- `file` is relative to the artifact's directory: no absolute path, no `.` or
  `..` component, no symbolic link on the way (refused as `Refused path ...`).
- `hash` is the 64 lowercase hex digits of the SHA-256 of the bytes last
  read; null or absent creates the file, with missing directories, and is
  refused if the file exists.
- The content is `text` (UTF-8) or `data` (base64 bytes), exactly one of the
  two, at most 16 MiB.

Success is `{"result": {"file", "hash", "bytes", "created"}}`, `hash` being
the new content's. A mismatch writes nothing and answers `{"error", "code":
"stale", "file", "hash"}` with the current hash (null for an absent file)
and, under 256 KiB, the current `bytes` and `text` or `data` as `read`
gives them. A consumer applying hunks (`ApplyPatch`) reads the file, applies
them to that content and writes the result with the hash it read; on a
stale answer it may reapply them to the returned content. `delete-file`
takes `file` and `hash` (required) and answers `{"result": {"file",
"deleted": true}}` or the same stale refusal; it refuses the primary file.
`files` lists `{"result": {"primary", "files": [{"file", "bytes"}]}}`.

A frame is the decoded JSON a sealed envelope carries, with one shorthand: an
editing request or reply, `{"t": "editing", "reqId": N, "body": VALUE}`,
stands for the chunked transfer `{"t": "editing", "reqId": N, "offset",
"total", "data"}` whose `data` slices join to base64 of VALUE's UTF-8 JSON.
Object key order is not significant.
