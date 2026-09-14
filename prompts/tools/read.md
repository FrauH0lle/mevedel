Read text or supported media from a file or resource address, subject to access
permissions and the current model's media capabilities.

### When to use `Read`

- Inspect a known file, code range, screenshot, or PDF.
- Retrieve packaged guidance at `mevedel://RELATIVE-PATH`.

### When NOT to use `Read`

- A filename pattern or directory listing: use Glob. Read does not expand
  wildcards or read filesystem directories.
- Searching contents across files: use Grep.

### How to use `Read`

- Relative paths start at the session working directory. Resource families:
  `work://`, `artifact://`, `skill://`, `agent://`, `history://`, `memory://`,
  `memory://journal/`, `mcp://`, and `mevedel://`. Read bare roots for available entries;
  the memory index is `memory://root`.
- Text returns numbered lines starting at 1, by default up to 2000 lines.
  Use `offset` and `limit` for a focused range; files over 512 KB require a
  range. Lines over 2000 characters and output over 50 Ki characters are
  truncated; follow the returned continuation guidance when present.
- Images (PNG, JPEG, GIF, WEBP) need model and backend image support. PDF
  document delivery also depends on backend support. An envelope alone does
  not mean the media was delivered; do not infer visual content from it.
- For PDF page rendering/resizing, media fallback, resource aliases, or JSON
  selection, read `mevedel://tools/files.md` before using those features.
  `offset` and `limit` apply only to text, never to images or PDFs.
- Missing, unreadable, or unsupported targets return an error. If a manual
  cannot be read, use the known ordinary contract or report missing guidance.

### Examples of good usage

<example>
- Inspect a function's implementation:
Read(file_path="src/utils.ts", offset=45, limit=18)
The result contains numbered source lines, possibly followed by truncation guidance.
</example>

### Examples of bad usage

<example>
Read(file_path="*test*")
<reasoning>
Read takes one target, not a pattern. Use Glob(pattern="**/*test*") to locate files.
</reasoning>
</example>
