Find where a function, variable, or class is used throughout your codebase.
Perfect for understanding code dependencies and impact analysis.

### When to use XrefReferences

- Finding all callers of a function before modifying it
- Understanding where a variable is read/written
- Impact analysis before refactoring
- Tracing data flow through a codebase
- Verifying that dead code is truly unused

### When NOT to use XrefReferences

- Searching for text patterns -> use Grep
- Looking for file names -> use Glob
- Getting an overview of a file -> use Imenu
- Complex multi-step searches -> delegate

### How to use XrefReferences

- Provide the exact identifier name (case-sensitive)
- Specify a file in the project for context (affects which xref backend is
  used)
- Works best with language servers (LSP), TAGS files, or built-in elisp xref
- More precise than grep for finding actual references vs. string matches

### Examples of good usage

<example>
- Find all calls to authenticate_user function
XrefReferences(identifier="authenticateUser", file_path="src/auth.ts")
</example>

### Examples of bad usage

<example>
XrefReferences(identifier="user", file_path=".")
<reasoning>
Too generic, might not be indexed as expected.
Use Grep for simple text searches instead.
</reasoning>
</example>
