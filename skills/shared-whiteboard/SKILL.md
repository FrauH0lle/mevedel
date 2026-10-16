---
name: shared-whiteboard
description: Create or substantially draw on a shared whiteboard with clear layout, consistent styling and readable flows. Use the Shared tools to edit native whiteboard elements; not for HTML artifacts or shared documents.
argument-hint: "[what the whiteboard should explain]"
user-invocable: true
---

$ARGUMENTS

# Shared whiteboards

Work on the requested whiteboard through SharedCreate/SharedEdit, not a separate
HTML preview. On an existing board, match its established style and ground;
do not restyle other people's work unless asked.

## Choose a treatment

On a new board, follow the user's requested look. Otherwise use **sketch** for
brainstorming, whiteboarding and informal boards; use **polished** for a diagram
intended as a deliverable or document figure. Keep one treatment per composition.

### Sketch — Excalidraw-style

Use a white canvas (`#ffffff`), hand-drawn strokes (`roughness: 1`),
`strokeWidth: 2`, `strokeStyle: "solid"`, `opacity: 100` and Excalifont
(`fontFamily: 5`). Keep most shapes unfilled (`backgroundColor: "transparent"`).
Use `strokeColor: "#1e1e1e"` for text and outlines; accents from the editor's
palette are blue `#1971c2`, red `#e03131`, green `#2f9e44`, orange `#f08c00`.
Pale fills are blue `#a5d8ff`, red `#ffc9c9`, green `#b2f2bb`, yellow `#ffec99`.
Use one or two accents, not every swatch. For coloured shapes, explicitly set
`fillStyle: "hachure"` for the editor's sketch texture; use `"solid"` for a quiet
filled region; sticky notes stay solid. Ink, transparent fill, roughness, width
and Excalifont are the defaults and may be omitted; **fillStyle and roundness**
are not: shapes you add default to solid and sharp, so round rectangles take
`roundness: {type: 3}` and diamonds `{type: 2}`.
Text sizes: 36 title, 28 headings, 20 node labels, 16 secondary labels.

### Polished — close to the HTML artifacts

Translate the artifact skills' visual intent, not their HTML/CSS mechanics:

| Role | Value |
| --- | --- |
| Canvas background, for a new board | `#f9f9f7` |
| Primary text and important outlines | `strokeColor: "#0b0b0b"` |
| Secondary text and ordinary connectors | `strokeColor: "#52514e"` |
| Accent for a key concept or path | `strokeColor: "#184f95"` |
| Quiet region fill | `backgroundColor: "#e3e8ed"` |
| Neutral node fill, when needed | `backgroundColor: "#fcfcfb"`; otherwise `"transparent"` |
| Meaningful success / failure only | `#1d7a4d` / `#b1382f`, with a label or symbol too |

Use `fillStyle: "solid"`, `roughness: 0`, `strokeStyle: "solid"`,
`opacity: 100` and consistent edges (`roundness: null` is a clean default).
Use `strokeWidth: 1` for quiet outlines and `2` for connectors.

Use `fontFamily: 6` (Nunito), with text sizes 32 for the title, 25 for section
headings, 20 for node labels and 16 for secondary labels.

## Compose for reading

In either treatment, colour encodes meaning; reinforce it with labels or symbols.
Set text colour through its own `strokeColor`. Use size and placement for
hierarchy, not unsupported font-weight fields.

Choose one reading direction. Align peers; keep equivalent nodes the same size.
Start with 26 units of padding, 52 between peers and 78 or more between groups.
Expand for labels and connector corridors rather than cramming the diagram.

Group related nodes with a quiet region or lane shape behind them, not a forest
of heavy borders. Keep labels short and bind text to its shape with `containerId`
so it wraps and moves with the node. Use the same label hierarchy throughout.

For flows, use arrows with `elbowed: true` and bind both `startBinding` and
`endBinding`. Leave corridors between shapes; let automatic routing avoid
obstacles rather than hand-placing waypoints or `fixedSegments`.

Discover reusable elements through `shared://library` when useful. Reuse sparingly:
keep editable elements and consistent styling, not a mixture of unrelated icon sets.

## Check the drawn result

Judge the PNG at displayed size: readable labels, useful grouping, no accidental
overlaps or confusing crossings. Improve the composition rather than shrinking text.

The PNG shows authored light colours, not the browser's dark theme. Both palettes
tolerate the viewer's dark-mode inversion; never store dark-theme hexes as a
second palette. Keep text in the dark neutral, not the pale fill colours. Faint
fills alone do not establish boundaries in both themes: retain an outline,
label or spacing cue where the distinction matters.
