Reads a file from the local filesystem. You can access any file directly by
using this tool. Assume this tool is able to read all files on the machine. If
the user provides a path to a file assume that path is valid. It is okay to read
a file that does not exist; an error will be returned.

Usage:
- The file_path parameter can be absolute or relative. Relative paths are
  resolved from the session working directory.
- By default, it reads up to 2000 lines starting from the beginning of the file.
- When you already know which part of the file you need, only read that part.
  This is important for larger files.
- `offset` and `limit` are line controls for text files only. Do not provide
  them when reading images or PDFs.
- Results are returned using cat -n format, with line numbers starting at 1
  for text files.
- This tool can only read files, not directories. To read a directory, use an ls
  command via the Bash tool.
- Any lines longer than 2000 characters will be truncated.
- Files over 512 KB in size can only be read by specifying offset and limit.
- Supported media files are PNG, JPG/JPEG, GIF, WEBP, and PDF.
- You will regularly be asked to read screenshots. If the user provides a path
  to a screenshot or other image file, use this tool to view it.
- Image reads require the current model to support media input. They return a
  compact media envelope in the transcript and attach native image input when
  the backend supports gptel media messages. If native attachment is not
  available, the model-visible result omits the base64 payload.
- PDF reads without `pages` return a base64 PDF envelope when the current model
  supports media input. Backends with native document support receive it as a
  document block; otherwise the model-visible result omits the base64 payload.
- PDF reads with `pages` render selected pages to images using `pdftoppm`
  (from poppler-utils). `pages` accepts `"3"`, `"1-5"`, or `"3-"`, with a
  maximum of 20 pages per request. Rendered pages follow the image-read path.
- For large PDFs or when the user asks for specific pages, use `pages` instead
  of reading the full PDF. If a full-PDF read fails because document media is
  unsupported, retry with `pages`.
- Media reads validate file contents against the declared extension before
  sending native image or document blocks.
- `max_width`, `max_height`, and `max_tokens` apply only to images and rendered
  PDF page images. They require ImageMagick (`magick` or `convert`); omit them
  when the original media can be sent as-is.

### When to use Read

- You need to examine file contents
- Before editing any file (required)
- You know the exact file path
- Inspecting known code ranges and implementation details

### When NOT to use Read

- Searching for files by name -> use Glob
- Searching file contents across multiple files -> use Grep
- Listing symbols in one known code file -> use Imenu
- Finding symbol definitions, callers, or usages -> use XrefDefinitions or
  XrefReferences

### How to use Read
- Default behavior reads from the beginning and stops at the built-in line and
  output caps
- For large files, use offset and limit parameters to read specific sections
- For a code outline, use Imenu first; for symbol definitions or references,
  use xref tools first, then Read the returned locations when details matter
- For PDFs, use `pages` when you need specific pages as images or when the PDF
  is likely too large to read as a single document
- For images or rendered PDF pages, use `max_width`/`max_height` only when the
  image is unnecessarily large
- Prefer focused ranges over whole-file reads when files are large or structured
- Always read before editing - edit tools will error otherwise
- You can call multiple tools in a single response. It is always better to
  speculatively read multiple potentially useful files in parallel.

### Examples of good usage

<example>
- Reading a specific function:
Read(file_path="src/utils.ts", offset=45, limit=18)
</example>

<example>
- Examining configuration before changes:
Read(file_path="config/database.yml")
</example>

<example>
- Inspecting PDF pages:
Read(file_path="docs/design.pdf", pages="2-4", max_width=1400)
</example>

<example>
- Sending an image to a media-capable model:
Read(file_path="screenshots/failure.png", max_width=1600)
</example>

### Examples of bad usage

<example>
- Trying to find all files with 'test' in the name:
Read(file_path="*test*")
<reasoning>
Should use Glob(pattern="*test*") instead.
</reasoning>
</example>
