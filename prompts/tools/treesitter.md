Inspect a file's tree-sitter syntax nodes, ranges, and hierarchy.

### When to use `Treesitter`

- Examine syntax at a location, its parents/children, or the whole syntax tree.

### When NOT to use `Treesitter`

- Looking for source text without needing syntax information.
- The file has no active tree-sitter parser; installing a grammar alone does
  not ensure the visiting buffer has a parser.

### How to use `Treesitter`

- Pass one existing file. The tool uses its visiting buffer's first parser,
  including unsaved contents. Missing parser/node and invalid positions are
  reported as errors.
- A line is 1-based and a column is 0-based. Without a line, inspect the buffer's
  current point. `whole_file=true` selects the root and ignores position and
  ancestor/child options.
- A location result includes node type, Emacs buffer-position range, short text,
  and available name/field details. Optional ancestry is limited to nine levels
  and direct children to twenty.
- Whole-file trees stop before depth 20 and have a roughly 200 Ki-character
  construction cap. Large results are persisted with a bounded preview and a
  retrieval address; omitted depth or truncated construction is not in that
  artifact.

### Examples of good usage

<example>
Treesitter(file_path="src/parser.js", line=10, column=5, include_ancestors=true)
-> Inspect the node and its enclosing syntax in a buffer with a working parser.
</example>

### Examples of bad usage

<example>
Treesitter(file_path="src/parser.js", line=0)
<reasoning>
Lines start at 1. Use line=1 for the first line, or whole_file=true to inspect
from the root.
</reasoning>
</example>
