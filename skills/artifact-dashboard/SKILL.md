---
name: artifact-dashboard
description: Create a dashboard artifact - KPI tiles, a chart, and a breakdown table for reading quantitative data at a glance. Use when the user asks for a dashboard, metrics view, KPI summary, monitoring page, or analytics overview. Only for CREATING a new dashboard; edits to an existing one modify its HTML directly.
argument-hint: "[what to show]"
user-invocable: true
---

$ARGUMENTS

!$artifact
!$artifact-design

# Dashboard artifacts

A dashboard is scanned and operated, not read top to bottom: the summary comes
before the detail, and what needs attention reads at a glance. The template
gives you a KPI row, one primary chart, and a breakdown table - a sensible
default arrangement, not a fixed structure.

## How to use

1. Read the template:

   ```
   ${MEVEDEL_SKILL_DIR}template.html
   ```

2. Copy it as your starting point and replace each `<!-- SLOT: ... -->` marker
   with real content; the comment inside each slot says what goes there. Each
   slot also carries placeholder values - replace those too, or delete the
   section they belong to.
3. Then make the dashboard fit the data and the ask: add charts, reorder or
   drop sections, extend the layout. The slots are where you start, not where
   you stop; the card, chart, and table styles are components to build with.
   Keep the base styling so the result reads as one coherent design.
4. Self-check before writing the file: no `SLOT` markers left, no placeholder
   or invented values, and every custom color routed through a token declared
   in every scope (light, both dark blocks, print) so it survives both themes.
5. Write the file into the session artifacts directory with ApplyPatch, per the
   artifact rules above.

**Creation only.** When updating an existing dashboard, work with its current
HTML directly - don't re-read or re-apply this template.

## Slots

| Slot | What to fill in |
| --- | --- |
| `TITLE` | The dashboard's name. Appears twice - the `<title>` element and the visible `<h1>`. Fill both. |
| `SUBTITLE` | The scope and period this covers. |
| `KPI_TILES` | 2-5 `.card.kpi` blocks, one headline number each, with an optional delta. |
| `CHART_TITLE` / `CHART_NOTE` | A meaningful chart heading and description: measure, units, categories, and any axis caveat. They label the accessible chart region. |
| `BREAKDOWN_TITLE` / `BREAKDOWN_ROWS` | The table heading, its `<th>` cells, and one `<tr>` per row. Put `class="num"` on both the `th` and the `td` of numeric columns. |
| `FOOTER_NOTE` | Data source and as-of date. |

The chart takes a JSON spec in the `chart-spec` script block, not markup: you
supply the data and a few knobs, and the bundled renderer owns the pixels. It
runs entirely offline without libraries or a publish-time runtime.

## The chart spec

```json
{
  "type": "line",
  "x": { "label": "Month" },
  "y": { "label": "USD", "min": null, "max": null },
  "series": [{ "name": "Revenue", "points": [{ "x": "Jan", "y": 1200 }] }]
}
```

- `"line"` (default when omitted) for a trend, `"bar"` for compared magnitudes,
  `"donut"` for parts of a whole. Unknown types are invalid, not line fallbacks.
  A donut reads `"slices": [{"name": "Product", "value": 1200}]` instead of
  `series`. Names must be nonempty strings; numbers must be finite JSON numbers,
  not numeric strings or booleans. Donut values must be nonnegative and their
  total finite. Zero-valued slices remain in the exact-values table and legend;
  an all-zero donut has a visible no-data state and retains its table.
- Every line/bar series needs a nonempty `name` and a `points` array. Every
  `x` is a unique, nonempty categorical string; all series must have exactly
  the same ordered categories. Categories are evenly spaced, **not a continuous
  time scale**. Do not imply proportional elapsed time for irregular samples.
  Explicit `y: null` in a line means missing data: it breaks the line, and
  isolated observations remain visible. Missing `y` or null bar values are
  invalid; never silently convert missing observations to zero.
- `x.label` / `y.label` are optional text captions. Put units in `y.label`:
  it also labels tooltip values and the exact table, including for donuts.
  The y ticks adapt precision to the scale; exact values retain the complete
  JavaScript number string without rounding, grouping, or abbreviation.
- `y.min` / `y.max` are optional finite numbers (`null` also means automatic).
  They must be increasing and contain every observation. Automatic domains
  include zero and cover both signs; bar bounds must always include zero.
  **Narrow line ranges far from zero** - uptime between 97% and 99% - flatten
  against a zero baseline: set bounds and disclose the truncation in
  `CHART_NOTE`. Bars are category-centered and never silently clipped.
- A malformed spec shows **Invalid chart data**, while empty arrays, all-null
  line data, and zero-total donuts show **No data to chart**. Numerically
  unrepresentable ranges or slice proportions are rejected visibly instead of
  generating broken geometry. It is strict JSON: no trailing commas or comments.
- Bars and donut slices use solid fills. Multiple series and donut slices use
  eight distinct categorical colors (`--chart-1` through `--chart-8`), independent
  of the page accent and semantic delta colors. All colors follow the live theme
  and have print values. Keep the numbered keys and matching legend; lines also
  use four repeating dash styles so identification does not depend on color
  alone. A single line/bar series uses `--accent` consistently and names itself
  in the chart title. Beyond eight series or slices, colors repeat: prefer
  separate charts or a table when comparisons become difficult to distinguish.
  Do not add hatching as a default substitute for distinct colors and labels.
- A shared text-only tooltip works on pointer hover and keyboard focus, and
  Escape dismisses it. Up to 40 marks are individually tabbable; denser charts
  use the exact table for keyboard navigation rather than hundreds of tab stops.
  The native **Exact chart values** disclosure is generated from the same
  validated data, works on touch without hover, and expands during printing
  (then restores its prior state). Do not remove it or hand-maintain a duplicate.
  Do not give the SVG `role="img"`, which would hide interactive descendants.
- Charts and tables scroll locally on narrow screens. Crowded x ticks are
  omitted and long labels abbreviated visually; complete categories remain in
  accessible labels, tooltips, and the exact table. Check the final content at
  narrow width and in both themes, including keyboard focus and print preview.
- Want a shape the spec doesn't cover? Hand-draw the SVG instead, reusing the
  card chrome and tokens; `artifact-diagramming` covers the mechanics.

## Rules that keep a dashboard honest

- **Replace every placeholder, and never invent one.** KPI numbers, table rows,
  the zeroed `REPLACE ME` series, the footer's source and date - each comes
  from the conversation or its section is removed. A dashboard of plausible
  fabricated numbers is worse than no dashboard.
- **No time dimension? Don't fabricate a trend.** Never invent a time axis for
  data that has none. Use `"bar"` or `"donut"` where that shape fits, or drop
  the chart and lead with the tiles and the table.
- **Format numbers for scanning.** KPI values get a unit and 2-3 significant
  figures with separators (`$1.2M`, `98.7%`, `412ms`); percentages get at most
  one decimal when that precision fits the data. These are display conventions,
  not permission to round the underlying chart spec or exact-values table.
  Keep a summary breakdown short; aggregate the tail into "Other" only for
  additive measures with a disclosed grouping rule. Never add rates, averages,
  medians, or percentiles; derive a valid weighted aggregate from its inputs or
  leave it unaggregated. The chart's exact table always retains every input row.
- **Keep the analysis consistent.** KPI tiles, charts, and breakdowns must use
  the same scope, period, units, denominators, and aggregation rules, or label
  the difference explicitly. Reconcile totals before rendering. State whether
  percentages use a 0-1 fraction or a 0-100 scale; the renderer does not convert
  units. Separate observed values from estimates or projections with named
  series and a clear note, never color alone. Do not imply unsupported precision,
  provenance, or certainty in the footer or a tooltip.
- **Color deltas by meaning, not direction.** `up`/`down` picks the arrow;
  `good`/`bad` picks the color. When a decrease is the improvement - latency,
  cost, error rate - mark it `good` so the color says whether the news is good.
  Semantic color is separate from the accent and doesn't count as one.
- **Encode state in form as well as number** - a pill, a chip, a severity
  stripe - so what needs attention survives a five-second glance.
