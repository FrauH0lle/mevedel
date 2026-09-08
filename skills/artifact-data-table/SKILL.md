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

A text-column filter, a dense sortable table under a header that stays put
while the rows scroll, and a live row count. The dataset is embedded as JSON
and the bundled renderer draws it. Filtering is a case-insensitive substring
search across text columns only, never numeric columns; while a filter is
active a scope line names it. Printing keeps the current filtered rows, count,
and that scope line - shown in print even when unfiltered - so a subset cannot
be mistaken for the whole dataset.

## How to use

1. Read the template:

   ```
   ${MEVEDEL_SKILL_DIR}template.html
   ```

2. Copy it as your starting point and replace each `<!-- SLOT: ... -->` marker
   with real content; the comment inside each slot says what goes there.
   Replace the placeholder column definitions and the `REPLACE ME` row too.
3. Self-check before writing the file: no `SLOT` markers left, no placeholder
   rows, and both JSON blocks parse and satisfy the data rules below. Check
   keyboard sorting, both sort directions, filtering, empty/no-match states,
   narrow-screen overflow, both color schemes, and print preview.
4. Write the file into the session artifacts directory with ApplyPatch, per the
   artifact rules above.

**Creation only.** When updating an existing table, work with its current HTML
directly - don't re-read or re-apply this template.

## Slots

| Slot | What to fill in |
| --- | --- |
| `TITLE` | The dataset's name. Appears twice - the `<title>` element and the visible `<h1>`. Fill both. |
| `SCOPE` | What this dataset covers and as of when. |
| `COLUMNS` | Nonempty JSON array of `{key, label, type}`. Keys are unique nonblank strings; `type` is `"text"` or `"num"`. Missing/null label defaults to the key; missing/null type defaults to text. |
| `ROWS` | JSON array of row objects keyed by the column keys. |
| `FOOTER_NOTE` | Data source, as-of date, and anything cut from the dataset. |

Data goes in the two JSON blocks, never as literal `<tr>` markup - the renderer
owns row emission, and hand-written rows are invisible to sort and filter.

## Data rules

These are where a table goes wrong quietly, so follow them exactly.

- **Columns and rows must have the documented shape.** Each column and row
  is an object, not `null` or an array. Column keys must be unique and nonblank;
  labels are scalar text (strings recommended), with missing/null labels
  falling back to the key. Missing/null type means `"text"`; a provided type
  must be `"text"` or `"num"`. Declared cells contain JSON scalars (strings,
  numbers, booleans, or `null`), not nested objects or arrays. Omitted cells
  are missing, including keys such as `constructor` or `__proto__`: sorting,
  filtering, and display read only a row's own properties.
- **Numbers in `"num"` columns are JSON numbers**, not strings: `1234.5`, never
  `"1,234.50"` or `"$1,234.50"`. Strip currency symbols and separators, and put
  the unit in the column label (`Amount (USD)`). A non-numeric value in a
  `"num"` column is shown as authored but skips formatting and sorts after
  finite numbers, before missing cells, in both directions. Numeric strings
  and booleans are never coerced to numbers.
- **A missing value is `null`** (or the key omitted) - never `0`, `"N/A"`, or
  `"-"`. Empty and whitespace-only strings count as missing too. Missing cells
  render blank and sort last in *both* directions, because absent is not
  extreme.
- **Dates go in `"text"` columns, ISO-8601** (`2026-07-08`), so alphabetical
  order is also chronological. `Jul 8, 2026` sorts wrong.
- **Both blocks must be strict JSON**: double quotes, no trailing commas, no
  comments, no `NaN` or `Infinity`. Invalid JSON or malformed shapes produce
  a visible load error, not a blank table. A valid empty row array shows
  `No rows.`; a filter excluding every row of a nonempty dataset shows
  `No rows match.`
- **Follow the shared artifact JSON-embedding rule:** after serialization,
  replace literal `<` with `\u003c`. It parses back to the original text while
  preventing the HTML parser from treating data as a closing script tag or
  comment. Strict JSON alone does not protect the enclosing page.
- **Pre-round to the precision worth showing.** Values display with up to six
  decimal places, and mixed precision makes right-aligned columns ragged.
- **Embed the whole dataset** up to a few thousand rows. Beyond that, subset or
  aggregate to what the user will actually browse, and say what was cut in
  `FOOTER_NOTE`. Every row ships to a guest's phone.

## Restyling

The template's value is its mechanics - layout, sorting, filtering. The styling
is a clean default, not a house style: restyle the whole `<style>` block when
the subject calls for it. Change a palette token in **every** scope that
declares it (the light `:root`, the `prefers-color-scheme: dark` block, the
`:root[data-theme="dark"]` block, and `@media print`), or it snaps back in one
theme or on paper.

Keep intact: the theming structure, table markup, `<script>` blocks, and the
ids and classes used by the renderer and styles - `dt`, `dt-filter`, `dt-count`,
`dt-filter-context`, `dt-columns`, `dt-rows`, `arrow`, `sorted`, `filtered`,
`num`, `empty`, `dt-none`, `table-wrap`. Keep `.table-wrap` a height-capped
scroll container: the sticky header only works inside a box that scrolls.
Preserve native sorting buttons inside `th scope="col"`,
header `aria-sort`, decorative arrows hidden from assistive technology, visible
keyboard focus, and the polite live count. Keep cell and query output as
`textContent`, never HTML. Retain wrapping controls and horizontal scrolling
on screen; print must use light colors, wrap cells without a clipped scroll
container, and retain the current filter description and row count.
