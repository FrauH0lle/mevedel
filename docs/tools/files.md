# Read, Glob, and Grep reference

The tool schemas and descriptions cover ordinary file reads and searches. This
manual covers resource addressing, media, and limits. It does not grant access
or change user scope. A missing manual is not permission to guess an advanced
contract; use the visible ordinary interface or report the missing guidance.

## Resource addresses and discovery

Addresses identify tool targets. Reading a skill does not invoke it, and
reading an agent result does not delegate work. Use advertised addresses rather
than inventing identities. Plain relative filesystem paths start at the session
working directory; addresses have their own roots. Markdown links and web URLs
are not filesystem paths.

| Family | Read | Glob / Grep | Target |
|---|---|---|---|
| `local://` | Yes | Yes | Shared session scratch files |
| `artifact://` | Yes | Yes | Persisted tool/execution output |
| `skill://` | Yes | Yes | Discovered skill packages |
| `agent://` | Yes | No | Latest settled retained-agent results |
| `history://` | Yes | No | Root and retained-agent conversations |
| `memory://root` | Yes | Yes | Configured memory index/roots |
| `mevedel://` | Yes | Yes | Installed Markdown documentation |
| `mcp://` | Yes | No | Connected servers' advertised resources |

Read a bare family address to list current entries, except memory uses
`memory://root`. For memory descendants, use the returned
`memory://ROOT-KEY/RELATIVE-PATH`; root keys may be readable names or hashes.
`history://root` reads the main conversation; it does not imply a corresponding
`agent://root` result. Availability is checked at use time, and missing sources
fail rather than being silently replaced.

Skill exact locators have the form `skill://NAME@SOURCE-KEY[/RELATIVE-PATH]`,
where SOURCE-KEY is the full lowercase SHA-256 source identity from discovery.
Readable aliases select the current discovered origin:

```text
skill://local-mevedel/SKILL
skill://local-agents/SKILL
skill://global-mevedel/SKILL
skill://global-agents/SKILL
skill://bundled/SKILL
skill://managed/SKILL
skill://plugin/PLUGIN/SKILL
```

Each accepts descendants such as `/README.md`. Results retain the authored
alias; a missing or ambiguous origin does not fall back to another skill.

MCP locators encode the entire server name and native resource URI as separate
URI components: `mcp://ENCODED-SERVER/ENCODED-RESOURCE-URI`. Encode internal
slashes, colons, fragments, percent signs, spaces, and Unicode. Reading
`mcp://ENCODED-SERVER` lists its advertised resources; it is not a filesystem
directory or a network access grant.

For lifecycle, containment, and full address semantics, consult
`mevedel://address-to-resource.md`.

## Selecting JSON from an agent result

If the complete settled payload is JSON, append a URI-fragment JSON Pointer:
`agent://root/reviewer#/findings/0/path`. The fragment is URI-decoded once,
then uses RFC 6901 escapes (`~1` for slash, `~0` for tilde). No fragment returns
ordinary result text; an empty fragment selects the complete parsed JSON value.
Missing pointers differ from JSON null. Fenced Markdown is not a JSON payload.
These selectors do not apply to arbitrary Markdown/manual addresses.

## Images, PDF pages, and resizing

Read supports PNG, JPG/JPEG, GIF, WEBP, and PDF after validating the file's
contents against its extension. Image delivery requires model media input and
backend image-message support. Full PDF delivery requires native document
support as well. A transcript media envelope is metadata, not proof that the
image or document reached the model; unsupported delivery omits base64 content.

PDF `pages` renders selected pages with `pdftoppm` from poppler-utils, then
uses the image-delivery path. Supported selections include `"3"`, `"1-5"`, and
`"3-"`, with at most 20 pages per request. Use this for selected pages or when
full-document delivery is unsupported and images are supported:

```text
Read(file_path="docs/design.pdf", pages="2-4")
```

`max_width`, `max_height`, and `max_tokens` resize images or rendered PDF pages
and require ImageMagick (`magick` or `convert`). Omit them when original media
is suitable. They are not text-output limits. `offset` and `limit` are text
line controls and cannot accompany image/PDF reads.

```text
Read(file_path="screenshots/failure.png", max_width=1600)
```

If a required renderer or media capability is unavailable, use another
supported representation when it answers the task or report the limitation.
Do not describe unseen visual contents. Resource families may be text-only;
in particular memory addresses reject binary/media reads.

## Text ranges and search limits

Read returns line numbers starting at 1 and defaults to at most 2000 lines.
Files above 512 KB need explicit range controls. Individual lines are truncated
at 2000 characters and final text output at 50 Ki characters. Use a returned
continuation offset when available; the end of a bounded result need not be
the end of the file. Unchanged duplicate reads may be suppressed in a session;
ToolCall nested reads still return the content needed by the script.

Glob returns up to 100 entries by default with a 30 KiB hard output cap.
Grep bounds output at 200 KiB, and content mode omits overlong lines according
to ripgrep's 2000-column limit. Grep's `head_limit` and zero-based `offset`
select returned lines/entries; they do not make an unbounded search cheap.
Normal oversized-result persistence can provide an artifact for further reads.

Both searches stop at the configured search timeout (20 seconds by default).
Timeout/output-limit messages identify partial output; narrow the path or
pattern before retrying. Result order is unspecified, so pagination across
changing searches is not a stable snapshot or proof of exhaustive coverage.

Glob includes hidden and ignored files. Grep includes hidden files but respects
ordinary traversal ignores; an explicit path or positive glob can select
ignored content. Both exclude version-control metadata. A directory-qualified
glob is relative to `path`; absolute patterns and `..` traversal are rejected.

Grep uses ripgrep regex syntax, not ToolCall's guest regex syntax. For example,
`^[(]defcustom` matches a literal opening parenthesis. Set `multiline=true` only
when the pattern needs to match across lines. Context controls apply to content
mode, not file lists or counts.
