# Commit messages

The repository combines a Conventional Commits-style subject with GNU-style
file and function entries. Describe the final change and its reason in present
tense. The [development guide](development.md) owns required validation before
committing.

## Format

```text
type(scope): Brief description

Optional explanation of the problem, change, and rationale.

* file.el (function, variable): Describe the change.

Optional trailers.
```

Use a 50–72-character subject when practical. Scope is optional; capitalize the
first word of the description. A breaking change places `!` after the type or
scope, as in `refactor(session)!: Replace the stored format`, and describes its
effect in a `BREAKING CHANGE:` footer.

## Body

Explain shared rationale once, then identify changed files and affected functions
or variables. Separate distinct changes in the same file with a blank line:

```text
* mevedel-example.el (example-start, example-stop): Share cleanup.

(example-status): Report the settled outcome.
```

A long affected-item list can continue by closing one parenthesis and opening
another on the next line. Use “Ditto” for an identical change in another file
when its meaning remains clear. Include relevant validation, limitations, and
compatibility effects; avoid claims unsupported by the actual checks.

These names are illustrative. A commit description should make the change
understandable without requiring the reader to reconstruct the conversation.

## Types

| Type | Use |
| --- | --- |
| feat | New capability |
| fix | Bug fix |
| docs | Documentation |
| style | Formatting without behavior changes |
| refactor | Code restructuring |
| perf | Performance improvement |
| test | Tests |
| build | Build system or dependencies |
| ci | Continuous integration |
| chore | Maintenance |
| revert | Reverting a change |
| tweak | Small user-facing default changes |

## Trailers

Separate trailers from the body with a blank line. Use `Token: value`, with
hyphens in multiword tokens (`Reviewed-by`, `Acked-by`); `BREAKING CHANGE` is the
exception. References and review attribution must identify actual records or
reviewers.

```text
BREAKING CHANGE: Existing saved records using the old schema are rejected.
Refs: <issue reference>
```

The upstream conventions are described in
[Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/),
[GNU change logs](https://www.gnu.org/prep/standards/html_node/Style-of-Change-Logs.html),
and [Git trailers](https://git-scm.com/docs/git-interpret-trailers).
