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

Use short, plain sentences to explain the problem and why the change fixes it.
Explain shared rationale once, then identify changed files and affected functions
or variables. Separate distinct changes in the same file with a blank line:

```text
* niwa-example.el (example-start, example-stop): Share cleanup.

(example-status): Report the settled outcome.
```

A long affected-item list can continue by closing one parenthesis and opening
another on the next line. The function list may start on the line after the
filename, and the description may start on the line after the colon. Files that
share a change may be grouped with commas or separate `* file` lines before one
function list and description:

```text
* a.el, b.el: Share the helper.

* longer-name.el
* other-long-name.el
(shared-function): Describe the shared change.
```

Use “Ditto” for an identical change in another file
when its meaning remains clear.

Summarize relevant checks and outcomes. Include reproduction steps when they
help the reviewer, rather than pasting routine test transcripts. Keep conditions
and limitations that affect the result, and note relevant compatibility effects.
Do not claim more than the checks establish.

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

## Reverts

Git's default `Revert "..."` subject and `This reverts commit HASH.` body are
accepted without rewording or GNU file entries. The default merge-revert body
also qualifies. A custom revert message follows the ordinary format, using the
`revert` type and GNU entries.

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
