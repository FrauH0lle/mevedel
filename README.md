# mevedel

mevedel is an Emacs package that adds a visual workflow for for interacting with
LLMs during programming. It is built around
[macher](https://github.com/kmontag/macher) and
[gptel](https://github.com/karthink/gptel), is versatile enough so that it can
be utilized in various types of buffers, and isn’t limited to just programming
buffers.

> [!WARNING]  
> The package is functional but still work in progress. Expect missing features, unexpected behavior and bugs.

## What does this package do?

### mevedel is macher plus evedel

This is a fork of the Emacs package
[evedel](https://github.com/daedsidog/evedel) which is unfortunately no longer
maintained.

`mevedel` aims to make [evedel](https://github.com/daedsidog/evedel)'s overlay
functionality and workflow available to the
[macher](https://github.com/kmontag/macher) package.


- Uses [macher](https://github.com/kmontag/macher) as a backend.
- Uses overlays for tracking instructions instead of raw text, decoupling
  instructions from your raw text. The overlays are mostly intuitive and can be
  customized, and try not to interfere with the regular Emacs workflow.
- Can save your instruction overlays so you won’t have to restart the labeling
  process each session.
- Can categorize your references with tags, and use complex query expressions to
  determine what to sent to the model in directives.
- Can easily cycle through between instruction overlays across all buffers.

<!-- TODO: Better video -->
https://github.com/user-attachments/assets/2799adab-5628-4ae3-b6a6-41171b4f87dd

## Requirements

- [macher](https://github.com/kmontag/macher) 0.3.0 or higher
- Emacs version 30.1 or higher

## Installation and configuration

The package is not available on MELPA but you can install it directly from
Github using [straight.el](https://github.com/radian-software/straight.el). 

``` emacs-lisp
(straight-use-package '(mevedel :host github :repo "FrauH0lle/mevedel" :files ("*.el")))

(use-package mevedel
  :after macher
  (mevedel-install))
```

`mevedel-install` is an interactive command and should be run whenever you
change one of the custom variables.

Run `mevedel-uninstall` to deactivate `mevedel`.

## Usage

mevedel’s function revolves around the creation and manipulation of references
and directives within your Emacs buffers. Directives are prompts you send to the
model, while references provide context to help complete these directives more
accurately.

<!-- TODO: Add video -->

### Management
| Command                           | Command Description                                                   |
|-----------------------------------|-----------------------------------------------------------------------|
| `mevedel-create-reference`        | Create or resize a reference instruction within a region.             |
| `mevedel-create-directive`        | Create or resize a directive instruction at point or within a region. |
| `mevedel-delete-instructions`     | Remove instructions either at point or within the selected region.    |
| `mevedel-delete-all-instructions` | Delete all mevedel instructions across all buffers.                   |

- If the region mark started from outside the reference/directive overlay and a
  part of it is within the selected region, the instruction will be "shrunk" to
  fit the new region boundaries.
- If the region mark started from inside the reference/directive overlay, the
  instruction will grow to include the entire new selection.

Below is an example of scaling existing instruction overlay (in this case, a
reference) by invoking the `mevedel-create-reference` command within a region
that contains one:

<!-- TODO: Add scaling video -->

### Saving & Loading

| Command                     | Command Description                                    |
|-----------------------------|--------------------------------------------------------|
| `mevedel-save-instructions` | Save current instruction overlays to a specified file. |
| `mevedel-load-instructions` | Load instruction overlays from a specified file.       |
| `mevedel-instruction-count` | Return the number of instructions currently loaded.    |

| Custom Variable                       | Variable Description                                             |
|---------------------------------------|------------------------------------------------------------------|
| `mevedel-patch-outdated-instructions` | Automatically patch instructions when the save file is outdated. |

The variable `mevedel-patch-outdated-instructions` controls the automatic
patching of instructions during loading when the save file is outdated. The
process is not perfect (word-wise diff), so you should always try and maintain a
separate instruction file per branch.

### Modification

| Command                               | Command Description                                               |
|---------------------------------------|-------------------------------------------------------------------|
| `mevedel-convert-instructions`        | Convert between reference and directive types at point.           |
| `mevedel-modify-directive`            | Modify an existing directive instruction at point.                |
| `mevedel-modify-reference-commentary` | Modify reference commentary at the current point.                 |
| `mevedel-add-tags`                    | Add tags to the reference under the point.                        |
| `mevedel-remove-tags`                 | Remove tags from the reference under the point.                   |
| `mevedel-modify-directive-tag-query`  | Enter a tag search query for a directive under the current point. |
| `mevedel-link-instructions`           | Link instructions by their ids.                                   |
| `mevedel-unlink-instructions`         | Unlink instructions by their ids.                                 |
| `mevedel-directive-undo`              | Undo the last change of the directive history at point.           |

| Custom Variable                            | Variable Description                                 |
|--------------------------------------------|------------------------------------------------------|
| `mevedel-empty-tag-query-matches-all`      | Determines matching behavior of queryless directives |
| `mevedel-always-match-untagged-references` | Determines matching behavior of untagged references  |

#### Categorization

<!-- TODO: Add tag query video -->

The categorization system in allows you to use tags to label and organize
references. You can add or remove tags to a reference using the commands
`mevedel-add-tags` and `mevedel-remove-tags`. Each tag is a symbolic label that
helps identify the nature or purpose of the reference.

You can also modify the tag query for a directive, which is a way to filter and
search for references by tags. The tag query uses an infix notation system,
allowing complex expressions with the operators `and`, `or`, and `not`. For
example, the query `signature and function and doc` means the directive should
match references tagged with `signature`, `function`, and `doc`. You may use
parentheses in these expressions.

Additionally, there are special meta tag symbols that have exclusive meanings:

- `is:bufferlevel`: Returns only references that contain the entire buffer.
- `is:tagless`: Returns references with no tags whatsoever.
- `is:directly-tagless`: Returns references which may have inherited tags, but
  no tags of their own.
- `is:subreference`: Returns references which have another reference as their
  parent.
- `is:with-commentary`: Returns references that directly contain commentary
  text.
- `id:<positive-integer>`: Returns references the id matched by
  `positive-integer`.

`mevedel-empty-tag-query-matches-all` determines the behavior of directives
without a tag search query. If set to `t`, directives lacking a specific tag
search query will use all available references. Alternatively, if set to `nil`,
such directives will not use any references, leading to potentially narrower
results.

`mevedel-always-match-untagged-references` controls the inclusion of untagged
references in directive prompts. When set to `t`, untagged references are always
incorporated into directive references, ensuring comprehensive coverage.
Conversely, when set to `nil`, untagged references are ignored unless
`mevedel-empty-tag-query-matches-all` is set to `t`.

#### Commentary

You can add commentaries to references with the
`mevedel-modify-reference-commentary` command. Commentaries can add extra context
and information to a reference. Example:

<!-- TODO: Add commentary example video -->

#### Linking

References can be linked to one another, which sets up a dependency or of
automatic inclusion relationship between the two. This means that when the first
reference is utilized, it will automatically bring into play the reference it is
linked to, as well. This chaining of references is recursive: if a linked
reference is itself linked to another, and so forth, all these links will be
followed automatically. This continues until either there are no more links to
follow or a cycle is detected in the linkage graph.

Linked references are also included when a directive is executed from within a
reference which links to another, in a similar fashion to commentaries.

Currently, linking is only relevant for references.

### Processing

| Command                            | Command Description                            |
|------------------------------------|------------------------------------------------|
| `macher-implement-directive`       | Implement directive via macher.                |
| `macher-revise-directive`          | Revise directive via macher.                   |
| `macher-discuss-directive`         | Discuss directive via macher.                  |
| `mevedel-preview-directive-prompt` | Preview directive prompt at the current point. |
| `mevedel-diff-apply-buffer`        | Apply the diff in the entire patch buffer.     |

| Custom Variable                     | Variable Description                                        |
|-------------------------------------|-------------------------------------------------------------|
| `mevedel-include-full-instructions` | Controls if instructions are fully included in the prompt   |
| `mevedel-show-patch-buffer`         | Controls if patch buffer should be displayed automatically  |
| `mevedel-show-action-buffer`        | Controls if action buffer should be displayed automatically |

You can use the `mevedel-preview-directive-prompt` command to do a dry-run and
see how the AI prompt will look like. Here's an example of previewing a
directive prompt:

<!-- TODO: Add preview directive example video -->

`macher-implement-directive`, `macher-revise-directive` or
`macher-discuss-directive` command will process the directive. 

Commands are also available via overlay actions. For example, you can preview
the patch before applying it or running an `ediff` session with the patch and
modify it to your liking.

### Navigation

| Command                               | Command Description                                         |
|---------------------------------------|-------------------------------------------------------------|
| `mevedel-next-instruction`            | Cycle through instructions in the forward direction.        |
| `mevedel-previous-instruction`        | Cycle through instructions in the backward direction.       |
| `mevedel-next-reference`              | Cycle through references in the forward direction.          |
| `mevedel-previous-reference`          | Cycle through references in the backward direction.         |
| `mevedel-next-directive`              | Cycle through directives in the forward direction.          |
| `mevedel-previous-directive`          | Cycle through directives in the backward direction.         |
| `mevedel-cycle-instructions-at-point` | Cycle through instructions at the point, highlighting them. |

### Customization

| Custom Variable                            | Variable Description                             |
|--------------------------------------------|--------------------------------------------------|
| `mevedel-reference-color`                  | Tint color for reference overlays                |
| `mevedel-directive-color`                  | Tint color for directive overlays                |
| `mevedel-directive-processing-color`       | Tint color for directives being processed        |
| `mevedel-directive-success-color`          | Tint color for successfully processed directives |
| `mevedel-directive-fail-color`             | Tint color for failed directives                 |
| `mevedel-instruction-bg-tint-intensity`    | Intensity for instruction background tint        |
| `mevedel-instruction-label-tint-intensity` | Intensity for instruction label tint             |
| `mevedel-subinstruction-tint-intensity`    | Coefficient for adjusting subinstruction tints   |

## Acknowledgments

- [daedsidog](https://github.com/daedsidog) for the [mevedel](https://github.com/daedsidog/mevedel) package
- [Kevin Montag](https://github.com/kmontag) for the [macher](https://github.com/kmontag/macher) package
- [Karthik Chikmagalur](https://github.com/karthink) for the [gptel](https://github.com/karthink/gptel) package
