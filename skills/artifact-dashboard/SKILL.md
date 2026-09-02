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
   or invented values, and every custom color routed through a token so it
   survives both themes.
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
| `CHART_TITLE` / `CHART_NOTE` | The chart's heading, and any axis caveat a reader needs. |
| `BREAKDOWN_TITLE` / `BREAKDOWN_ROWS` | The table heading, its `<th>` cells, and one `<tr>` per row. Put `class="num"` on both the `th` and the `td` of numeric columns. |
| `FOOTER_NOTE` | Data source and as-of date. |

The chart takes a JSON spec in the `chart-spec` script block, not markup: you
supply the data and a few knobs, and the bundled `renderChart` owns the pixels.

## The chart spec

```json
{
  "type": "line",
  "y": { "min": null, "max": null },
  "series": [{ "name": "Revenue", "points": [{ "x": "Jan", "y": 1200 }] }]
}
```

- `"line"` (default) for a trend, `"bar"` for compared magnitudes, `"donut"`
  for parts of a whole. A donut reads `"slices": [{"name": ..., "value": ...}]`
  instead of `series`.
- Multiple series each get a stroke stepped off the accent and an automatic
  legend. A single series names itself in the chart title; don't add a legend
  for it.
- `y.min` / `y.max` override the default zero-floored domain. **Narrow ranges
  far from zero** - uptime between 97% and 99% - flatten against a zero floor,
  so zoom the domain and say so in `CHART_NOTE`, or the zoom misleads.
- The renderer skips the chart entirely rather than drawing something wrong
  when the spec is empty or unparseable. If the chart doesn't appear, the spec
  is malformed - it is strict JSON, so no trailing commas and no comments.
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
  one decimal. Keep the breakdown to roughly ten rows and roll the tail into an
  "Other" row.
- **Color deltas by meaning, not direction.** `up`/`down` picks the arrow;
  `good`/`bad` picks the color. When a decrease is the improvement - latency,
  cost, error rate - mark it `good` so the color says whether the news is good.
  Semantic color is separate from the accent and doesn't count as one.
- **Encode state in form as well as number** - a pill, a chip, a severity
  stripe - so what needs attention survives a five-second glance.
