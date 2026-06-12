Get tree-sitter syntax tree information for a file, including node types,
ranges, and hierarchical structure. Useful for understanding code structure and
AST analysis.

### When to use Treesitter

- Analyzing precise syntax structure of code
- Understanding code hierarchy and nesting
- Extracting structured information about code elements
- Working with complex syntax that needs precise parsing

### When NOT to use Treesitter

- Simple text search -> use Grep
- Just reading code -> use Read
- Getting a simple overview of functions -> use Imenu (simpler and faster)
- Language does not have tree-sitter support in Emacs
- You do not need detailed syntax tree information

### How to use Treesitter

- Provide the file path and optionally a line/column position
- Only works for languages with tree-sitter grammar installed in Emacs
- Returns detailed syntax tree structure
- More detailed than Imenu but also more complex
- Best for tasks requiring precise syntactic analysis

### Examples of good usage

<example>
- Analyze syntax tree at specific location
Treesitter(file_path="src/complex-parser.js", line=10, column=5)
</example>

<example>
- Understand complex YAML structure
Treesitter(file_path="nested-config.yaml", whole_file=true)
</example>

### Examples of bad usage

<example>
Treesitter(file_path="README.md")
<reasoning>
Simple text document.
Use Read to read documentation files.
</reasoning>
</example>
