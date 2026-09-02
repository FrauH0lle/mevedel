---
name: artifact-data-table
description: Create a data-table artifact - a sortable, filterable table for browsing a dataset (a CSV, a list of records, query results, a catalog) rather than seeing it summarized. Only for CREATING a new table; edits to an existing one modify its HTML directly.
argument-hint: "[what to tabulate]"
user-invocable: true
---

$ARGUMENTS

!$artifact
!$artifact-design

# Data-table artifacts

A filter box, a dense sortable table, and a row count. The dataset is embedded
as JSON and the bundled renderer draws it, so sorting and filtering stay
correct no matter how many rows there are.

## How to use

1. Read the template:

   ```
   ${MEVEDEL_SKILL_DIR}template.html
   ```

2. Copy it as your starting point and replace each `<!-- SLOT: ... -->` marker
   with real content; the comment inside each slot says what goes there.
   Replace the placeholder column definitions and the `REPLACE ME` row too.
3. Self-check before writing the file: no `SLOT` markers left, no placeholder
   rows, and both JSON blocks parse.
4. Write the file into the session artifacts directory with ApplyPatch, per the
   artifact rules above.

**Creation only.** When updating an existing table, work with its current HTML
directly - don't re-read or re-apply this template.

## Slots

| Slot | What to fill in |
| --- | --- |
| `TITLE` | The dataset's name. Appears twice - the `<title>` element and the visible `<h1>`. Fill both. |
| `SCOPE` | What this dataset covers and as of when. |
| `COLUMNS` | JSON array of `{key, label, type}`. `type` is `"text"` or `"num"`. |
| `ROWS` | JSON array of row objects keyed by the column keys. |
| `FOOTER_NOTE` | Data source, as-of date, and anything cut from the dataset. |

Data goes in the two JSON blocks, never as literal `<tr>` markup - the renderer
owns row emission, and hand-written rows are invisible to sort and filter.

## Data rules

These are where a table goes wrong quietly, so follow them exactly.

- **Numbers in `"num"` columns are JSON numbers**, not strings: `1234.5`, never
  `"1,234.50"` or `"$1,234.50"`. Strip currency symbols and separators, and put
  the unit in the column label (`Amount (USD)`). A non-numeric value in a
  `"num"` column is shown as authored but skips formatting and sorts last.
- **A missing value is `null`** (or the key omitted) - never `0`, `"N/A"`, or
  `"-"`. Empty and whitespace-only strings count as missing too. Missing cells
  render blank and sort last in *both* directions, because absent is not
  extreme.
- **Dates go in `"text"` columns, ISO-8601** (`2026-07-08`), so alphabetical
  order is also chronological. `Jul 8, 2026` sorts wrong.
- **Both blocks must be strict JSON**: double quotes, no trailing commas, no
  comments, no `NaN` or `Infinity`. The renderer draws nothing at all rather
  than something wrong when a block fails to parse, so an empty table means
  malformed JSON.
- **Escape `</` as `<\/` and `<!--` as `<\u0021--`** inside JSON string values.
  Both are valid JSON and parse back to the original text. Left raw,
  `</script` ends the block early - breaking the table and rendering the rest
  of the value as live HTML.
- **Pre-round to the precision worth showing.** Values display with up to six
  decimal places, and mixed precision makes right-aligned columns ragged.
- **Embed the whole dataset** up to a few thousand rows. Beyond that, subset or
  aggregate to what the user will actually browse, and say what was cut in
  `FOOTER_NOTE`. Every row ships to a guest's phone.

## Restyling

The template's value is its mechanics - layout, sorting, filtering. The styling
is a clean default, not a house style: restyle the whole `<style>` block when
the subject calls for it. Change a palette token in **all three** scopes that
declare it (the light `:root`, the `prefers-color-scheme: dark` block, and
`@media print`), or it snaps back in dark mode or on paper.

Keep intact: the theming structure, the table markup, the `<script>` blocks,
and the ids and classes the script reads - `dt`, `dt-filter`, `dt-count`,
`dt-columns`, `dt-rows`, `arrow`, `sorted`, `num`, `empty`. Renaming any of
them breaks sorting, filtering, or theming.
