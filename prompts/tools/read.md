Read text or supported media from a file or resource address, subject to access
permissions and the current model's media capabilities.

Use Glob for filename patterns/directories and Grep for content searches.
Read does not expand wildcards or read directories.

- Relative paths start at the session working directory. Resource families:
  `work://`, `artifact://`, `skill://`, `agent://`, `history://`, `memory://`,
  `memory://journal/`, `mcp://`, `shared://`, and `mevedel://`. Read bare roots for available entries;
  the memory index is `memory://root`.
- Text returns numbered lines starting at 1, by default up to 2000 lines.
  Use `offset` and `limit` for a focused range; files over 512 KB require a
  range. Lines over 2000 characters and output over 50 Ki characters are
  truncated. Each truncated line gives an exact Read continuation using
  `char_offset` (zero-based decoded characters, not bytes). A positive
  `char_offset` selects one line; `limit` must be 1 or omitted. Follow the
  returned guidance to retrieve the rest without raising the output caps.
- Images (PNG, JPEG, GIF, WEBP) need model and backend image support. PDF
  document delivery also depends on backend support. An envelope alone does
  not mean the media was delivered; do not infer visual content from it.
- For PDF page rendering/resizing, media fallback, resource aliases, or JSON
  selection, read `mevedel://tools/files.md` before using those features.
- Missing, unreadable, or unsupported targets return an error. If a manual
  cannot be read, use the known ordinary contract or report missing guidance.

Read(file_path="src/utils.ts", offset=40, limit=30) requests lines 40-69.
A bounded page is not proof of end-of-file; follow returned continuation hints.
