# Commit Summary

Generate a git commit summary line following Conventional Commits style from the
provided diff. Follow the instructions below.

## Basic format:
```
type(component): brief description (50-72 chars)
```

## Guidelines

- The summary line should be 50-72 characters or less, and
- it must begin with with the type and the optional package/component/scope/file
  name within parenthesis.
- This should be followed by a colon and space (": ") then a summary of the change.
- The change summary should begin with a verb in active tense, for example: "Fix", "Update", "Add", "Remove" etc.
- The first word of the title should be capitalized.
- The summary can use common abbreviations to keep the line width under 72 characters.
- The summary must capture the intent of the change, and not just restate details of the changes.

If the changes are across many files and appear to be unconnected or disparate,
mention the important ones with commas while staying under 72 characters.
Example:

`foo: Add X, fix Y, remove Z from W`

## Conventional Commits Types:

| Type     | Description                                                       |
|:---------|:------------------------------------------------------------------|
| feat     | New feature addition                                              |
| fix      | Bug fix                                                           |
| docs     | Documentation changes                                             |
| style    | Code style/formatting changes (no code change)                    |
| refactor | Code refactoring                                                  |
| perf     | Performance improvements                                          |
| test     | Adding/fixing tests                                               |
| build    | Build system changes                                              |
| ci       | CI configuration changes                                          |
| chore    | General maintenance                                               |
| revert   | Revert previous changes                                           |
| tweak    | Code changes that change user-facing defaults but not drastically |

## Examples of good summary lines:

`feat(parser): Add finite state machine`
`fix(api): Prevent racing of requests`
`tweak(gptel): Reduce Windows curl-file-size-threshold`

## Output

Generate ONLY the git commit summary line without any explanation or markdown
code fences. Do not ask for further clarification, and make any assumptions you
need to follow instructions.
